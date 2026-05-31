(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("latexrelease" "2020-02-02") ("appendix" "toc" "page") ("geometry" "a4paper" "bindingoffset=0.2in" "left=1in" "right=1in" "top=0.75in" "bottom=0.75in" "footskip=.25in")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "tikz/sample"
    "tikz/random_walk_example_junction_tree"
    "tikz/random_walk_example"
    "tikz/graph_1"
    "tikz/jtree"
    "tikz/jtree_a"
    "tikz/proof_example"
    "latexrelease"
    "article"
    "art12"
    "star"
    "tikzit"
    "natbib"
    "appendix"
    "rotating"
    "geometry")
   (TeX-add-symbols
    '("press" 1)
    '("Ts" 2)
    '("Tn" 2)
    '("Tbd" 2)
    '("edge" 1)
    '("keywords" 1)
    "nodea"
    "nodeb"
    "nseta"
    "nsetb"
    "deg"
    "neig"
    "bd"
    "leaf"
    "adj"
    "pos")
   (LaTeX-add-labels
    "sec:expand-junct-trees"
    "eq:running-intersection-property"
    "eq:junction-property"
    "fig:example_random_walk_graph"
    "eq:joint-csf"
    "sec:decomp-graphs-random-walks"
    "eq:hierarchical-model"
    "alg:random-walk"
    "fig:example_random_walk"
    "thm:1"
    "sec:graph-perturbations"
    "eq:nei-bound-clique-basic-no-cond"
    "fig:example1_graph"
    "example:1"
    "fig:example1_junction"
    "eq:congruence"
    "lem:connect"
    "lem:disconnect"
    "sec:single-move-sampler"
    "eq:proposal-basic"
    "eq:reverse-proposal-connect"
    "eq:reverse-proposal-disconnect"
    "sec:graph-prior-post"
    "eq:csf"
    "lem:jtree-csf"
    "eq:joint-jtree-csf"
    "eq:junction-prior-cfs"
    "eq:acceptance-prop-single-move"
    "sec:jt-sampler"
    "eq:proposal"
    "eq:acceptance-prop"
    "sec:comp-cons"
    "tb:update-cases"
    "sec:numerical"
    "sec:simul-setup-gauss"
    "eq:precision"
    "fig:ggm-ture-G"
    "sec:gaussian-data-with"
    "fig:ggm-map"
    "fig:ggm_accpt_ratio"
    "sec:comp-graph-doma"
    "fig:gt-parallel-traceplot"
    "fig:boxplot-acceptance-rate"
    "proof:lem:connect-disconnect"
    "proof:lem:connect"
    "fig:disconnect-x-y"
    "sec:infer-setup-gauss"
    "eq:posterior-ggm"
    "app:sec:ggm"
    "fig:prior-comparison"
    "fig:time-samplers"
    "fig:gt_parallel_size_traceplot")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

