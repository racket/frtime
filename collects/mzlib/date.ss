
(require-library "dateu.ss")
(require-library "functiou.ss")

(invoke-open-unit/sig 
 (compound-unit/sig
  (import)
  (link [date@ : mzlib:date^ (mzlib:date@ function@)]
	[function@ : mzlib:function^ (mzlib:function@)])
  (export (open date@)))
 #f)

 
