-- =============================================================================
-- Lyft SQL sketch: build a trips.csv-like edge × timestamp dataset
-- =============================================================================
--
-- Target shape (traveltimeCLT / traveltimeHMM "trips" / QCD sample):
--   trip, time, timeBins, tt, logspeed, length, linkId, src, osm, dst
--
-- Package references:
--   traveltimeHMM::tripset  -> tripID, linkID, timeBin, logspeed, traveltime,
--                              length, time
--   traveltimeCLT::trips    -> trip, time, timeBins, tt, logspeed, length,
--                              linkId, src, osm, dst
--
-- QCD context (paper): map-matched GPS on a fixed road graph; one row per
-- (trip, directed edge) with segment entry time, length (m), travel time (s),
-- and log-speed = ln(length / tt).
--
-- Lyft terminology (important):
--   "ETA segment"  = one leg of an ETA estimate (driver -> pickup, pickup -> dropoff,
--                    or to the next waypoint). NOT one graph edge.
--   "DRNS / edge"  = one directed road-network link (~ QCD linkId).
--
-- WRONG source for trips.csv-like edge rows:
--   eta.eta_segments, default.fact_eta_segments_no_flags (FEBF)
--   These inherit ETA-segment granularity (~ pickup + dropoff legs, plus re-estimation
--   cycles). drns_id there is tied to segment-end / ETA evaluation, not the full
--   traversed edge sequence. See mappingdocs FEBF overview.
--
-- RIGHT source for all traveled graph edges (closest to QCD map-matching):
--   locations.drv_ride_locations_v2  (EORMM offline: GPS -> directional DRNS chain)
--     Used by SSMC / route_overlap_v3 "eormm_offline_actuals"
--     Docs: mappingdocs.lyft.net/dataeng/data_architecture/noncompliance_framework/
--   Alternative (online matching during ride):
--     default.event_distance_calculated
--
-- IMPORTANT:
--   1. Verify exact column / array names in Amundsen before running.
--   2. Replace {{start_ds}}, {{end_ds}}, {{region}} below.
--   3. drns_id (Directional Road Network Segment) ~ QCD linkId / graph edge.
--   4. src/osm/dst are QCD graph topology fields; Lyft does not expose OSM ids in
--      EORMM/FEBF directly — leave NULL or join a map-version graph table if needed.
--
-- Run in: sql.lyft.net (Trino/Presto) or Mozart
-- =============================================================================

-- ---------------------------------------------------------------------------
-- Parameters (edit before running)
-- ---------------------------------------------------------------------------
-- {{start_ds}}  e.g. '2025-01-01'
-- {{end_ds}}    e.g. '2025-01-07'
-- {{region}}    e.g. 'SFO'  (see coco.fact_rides / FEBF region column)
-- {{max_trips}} optional cap for prototyping, e.g. 5000

-- ---------------------------------------------------------------------------
-- Query A (recommended): EORMM map-matched actual route -> trips.csv-like rows
-- ---------------------------------------------------------------------------
-- One output row per (ride, traversed DRNS / graph edge).
-- Source: locations.drv_ride_locations_v2 (end-of-ride GPS map matching).
-- SSMC stage "EORMM Enriched" explodes directional segment lists with timestamps.

WITH params AS (
  SELECT
    CAST('{{start_ds}}' AS VARCHAR) AS start_ds,
    CAST('{{end_ds}}' AS VARCHAR) AS end_ds,
    CAST('{{region}}' AS VARCHAR) AS region,
    CAST({{max_trips}} AS BIGINT) AS max_trips
),

rides AS (
  SELECT
    r.ride_id,
    r.region,
    r.requested_at,
    r.picked_up_at,
    r.dropped_off_at
  FROM coco.fact_rides r
  CROSS JOIN params p
  WHERE r.ds BETWEEN p.start_ds AND p.end_ds
    AND r.region = p.region
    AND r.ride_status = 'finished'
    AND r.picked_up_at IS NOT NULL
    AND r.dropped_off_at IS NOT NULL
  ORDER BY r.requested_at
  LIMIT (SELECT max_trips FROM params)
),

-- VERIFY all column / array names in Amundsen:
--   amundsen.lyft.net -> locations.drv_ride_locations_v2
-- Typical shape (names vary): parallel arrays of drns_id, entry_time, duration, distance
-- per edge along the map-matched route; possibly separate pickup vs dropoff legs.
eormm AS (
  SELECT
    l.ride_id,
    l.ds
    -- , l.leg_type                    -- VERIFY: pickup / dropoff / full_trip
    -- , l.drns_ids                    -- VERIFY: ARRAY<BIGINT> traversed edges in order
    -- , l.segment_entry_at            -- VERIFY: ARRAY<TIMESTAMP> edge entry times
    -- , l.segment_duration_secs       -- VERIFY: ARRAY<DOUBLE> per-edge travel time
    -- , l.segment_distance_meters     -- VERIFY: ARRAY<DOUBLE> per-edge length
  FROM locations.drv_ride_locations_v2 l
  INNER JOIN rides r
    ON l.ride_id = r.ride_id
  CROSS JOIN params p
  WHERE l.ds BETWEEN p.start_ds AND p.end_ds
),

-- UNNEST parallel arrays into one row per traversed edge.
-- Replace column names after Amundsen check; keep ordinality for ordering.
edges AS (
  SELECT
    e.ride_id,
    e.ds,
    edge.ord AS seg_idx,
    edge.drns_id,
    edge.entry_time AS time,
    edge.duration_secs AS tt,
    edge.distance_meters AS length
  FROM eormm e
  CROSS JOIN UNNEST(
    -- zip_arrays(...) if your engine supports it; otherwise use WITH ORDINALITY on
    -- a ROW-type array built in a staging view. Pseudocode:
    zip_with(
      e.drns_ids,                       -- VERIFY
      e.segment_entry_at,               -- VERIFY
      e.segment_duration_secs,            -- VERIFY
      e.segment_distance_meters,          -- VERIFY
      (d, t, dur, dist) -> ROW(d, t, dur, dist)
    )
  ) WITH ORDINALITY AS edge(drns_id, entry_time, duration_secs, distance_meters, ord)
  WHERE edge.drns_id IS NOT NULL
    AND edge.drns_id <> 0
    AND edge.duration_secs > 0
    AND edge.distance_meters > 0
    -- Optional: keep only passenger trip leg after pickup
    -- AND e.leg_type = 'dropoff'
),

ordered AS (
  SELECT
    e.*,
    DENSE_RANK() OVER (ORDER BY r.requested_at, e.ride_id) AS trip
  FROM edges e
  INNER JOIN rides r
    ON e.ride_id = r.ride_id
),

with_topology AS (
  SELECT
    o.trip,
    o.time,
    CASE
      WHEN day_of_week(o.time) IN (6, 7) THEN 'Weekendday'
      WHEN day_of_week(o.time) NOT IN (6, 7)
           AND hour(o.time) + minute(o.time) / 60.0 >= 6.5
           AND hour(o.time) + minute(o.time) / 60.0 < 8.5
        THEN 'MorningRush'
      WHEN day_of_week(o.time) NOT IN (6, 7)
           AND hour(o.time) + minute(o.time) / 60.0 >= 15.5
           AND hour(o.time) + minute(o.time) / 60.0 < 17.0
        THEN 'EveningRush'
      WHEN hour(o.time) >= 20 OR hour(o.time) < 6
        THEN 'EveningNight'
      ELSE 'Weekday'
    END AS timeBins,
    o.tt,
    LN(o.length / o.tt) AS logspeed,
    o.length,
    CAST(o.drns_id AS BIGINT) AS linkId,
    CAST(NULL AS BIGINT) AS src,
    CAST(NULL AS BIGINT) AS osm,
    CAST(NULL AS BIGINT) AS dst
  FROM ordered o
)

SELECT
  trip,
  time,
  timeBins,
  tt,
  logspeed,
  length,
  linkId,
  src,
  osm,
  dst
FROM with_topology
ORDER BY trip, time, seg_idx;


-- ---------------------------------------------------------------------------
-- Query B (NOT for trips.csv): eta.eta_segments / FEBF — ETA legs only
-- ---------------------------------------------------------------------------
-- eta.eta_segments infers pickup/dropoff ETA segments from dispatch + ride events.
-- You typically get ~1 pickup + ~1 dropoff row per ride (plus re-estimation cycles),
-- NOT the full list of traversed DRNS edges. Same for fact_eta_segments_no_flags.
-- Use these only for ETA model evaluation, not traveltimeCLT edge-level trips.
--
-- FEBF dropoff-leg sketch (aggregate leg timing, still NOT per-edge):
/*
WITH params AS (
  SELECT CAST('{{start_ds}}' AS VARCHAR) AS start_ds,
         CAST('{{end_ds}}' AS VARCHAR) AS end_ds,
         CAST('{{region}}' AS VARCHAR) AS region
)
SELECT
  f.ride_id,
  f.segment_id,
  f.segment_end_id_rnk,
  f.drns_id,                 -- segment-end DRNS, not full route
  f.started_at_act,
  f.duration_secs_act,
  f.distance_meters_act
FROM default.fact_eta_segments_no_flags f
INNER JOIN coco.fact_rides r ON f.ride_id = r.ride_id
CROSS JOIN params p
WHERE f.ds BETWEEN p.start_ds AND p.end_ds
  AND r.region = p.region
  AND f.is_valid_segment_version = TRUE
  -- VERIFY segment type column (pickup / dropoff)
  AND f.segment_type = 'dropoff';
*/


-- ---------------------------------------------------------------------------
-- Query C (export): write result to S3 for local R workflows
-- ---------------------------------------------------------------------------
-- Example pattern after validating Query A on a small date range:
--
-- CREATE TABLE hive.user_scratch.trips_like_{{user}}_{{start_ds}}_{{end_ds}}
-- WITH (format = 'CSV', external_location = 's3://.../trips_like/')
-- AS
-- <paste Query A body here>;
--
-- Then download and point simulations at:
--   fread("data/trips.csv")
-- with columns already aligned.


-- ---------------------------------------------------------------------------
-- Column mapping cheat sheet
-- ---------------------------------------------------------------------------
-- trips.csv          | tripset (HMM)     | Lyft (EORMM / map-matched route)
-- -------------------|-------------------|----------------------------------
-- trip               | tripID            | DENSE_RANK(ride_id) or ride_id hash
-- time               | time              | per-edge entry time from EORMM arrays
-- timeBins           | timeBin           | derived from time (see CASE above)
-- tt                 | traveltime        | per-edge duration from EORMM arrays
-- logspeed           | logspeed          | ln(length / tt)
-- length             | length            | per-edge distance from EORMM arrays
-- linkId             | linkID            | drns_id (directional graph edge)
-- src                | —                 | graph start vertex (optional join)
-- osm                | —                 | not in EORMM; optional map join
-- dst                | —                 | graph end vertex (optional join)
--
-- R rename for traveltimeCLT (see case_study.rmd / table4 Rmds):
--   names(trips)[c(2,3,5,7,8)] <- c(
--     "tripID", "entry_time", "duration_secs", "distance_meters", "linkID"
--   )
--   trips$speed <- exp(trips$logspeed)
--   trips$timeBin <- time_bins_readable(trips$entry_time)
