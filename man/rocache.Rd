% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rocachePackageDocumentation.R
\docType{package}
\name{rocache}
\alias{rocache}
\title{rocache: Efficient Caching for R Objects}
\description{
The 'rocache' package offers a powerful caching mechanism that ensures the reproducibility of R results. It provides control to the user, allowing them to avoid rerunning time-consuming code by checking for changes in input/output.

Key Features:
\itemize{
\item Effortless Reproducibility: 'rocache' enables easy tracking and verification of code outputs by caching and comparing results, ensuring consistent and reproducible analyses.
\item Time-saving Efficiency: By storing and retrieving computed results, 'rocache' significantly reduces execution time by avoiding redundant computations.
\item Customizable Control: The caching mechanism in 'rocache' puts you in charge, allowing you to specify the caching behavior and expiration policies that best suit your workflow.
\item Seamless Integration: 'rocache' integrates smoothly with your existing code and workflows, minimizing any additional effort required to implement caching.
}

Whether you are working with computationally intensive tasks, complex simulations, or large datasets, 'rocache' empowers you to optimize your code and streamline your analyses by intelligently managing cached results.
\subsection{Exported function}{

cFolder, dFile, defaultFuncHandle, digestOfFunc, initCache, listCache, overwrite, overwrite<-, packageVersion_to_dFileVersion, pkgname, prevVersion, rdsVersion, readCache, removeCache, storeCache, synchronizeCache, version
}
}
\keyword{R}
\keyword{caching}
\keyword{efficiency}
\keyword{reproducibility}
