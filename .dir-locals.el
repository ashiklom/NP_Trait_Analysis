;; Use drake for compilation
((ess-r-mode . ((ess-r-package-dirs . (("R" . 1) ("tests" . 1) ("testthat" . 2)
                                       ("scripts" . 1) ("manuscript-analyses" . 2) ("presentations" . 2)))))
 (nil . ((compile-command . "Rscript scripts/drake.R"))))
