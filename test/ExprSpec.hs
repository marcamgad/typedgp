-- | Tests for the AST, the interpreter and the pretty-printer.
--
-- Expected values are hand-computed and written as literals. That is the
-- point: a test that recomputes the answer with the same code it is
-- testing proves only that the code is consistent with itself.
module ExprSpec (tests) where

import TypedGP.Eval
  ( eval
  , evalChecked
  , gammaFunction
  , magnitudeCap
  , protectedDiv
  , protectedExp
  , protectedGamma
  , protectedLog
  , protectedPow
  , protectedSqrt
  , protectedZeta
  , sanitize
  , zetaEulerMaclaurin
  )
import TypedGP.Expr
  ( Expr (..)
  , arity
  , isNonIntegerConst
  , provablyNonNegative
  , countNodes
  , depth
  , flatten
  , isTerminal
  , nodeDepths
  , opName
  , replaceAt
  , subtreeAt
  , variablesOf
  )
import TypedGP.Pretty (pretty)

-- | @2x + sin(y)@ — the benchmark formula, used as the standard fixture.
--
-- Pre-order layout, which every index below refers to:
--
-- > 0 Add
-- > 1   Mul
-- > 2     Const 2
-- > 3     Var x
-- > 4   Sin
-- > 5     Var y
sample :: Expr
sample = Add (Mul (Const 2.0) (Var "x")) (Sin (Var "y"))

-- | A value that is NaN, built at runtime so no literal NaN is needed.
notANumber :: Double
notANumber = 0.0 / 0.0

positiveInfinity :: Double
positiveInfinity = 1.0 / 0.0

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 1.0e-9

tests :: [(String, Bool)]
tests =
  -- Structure -------------------------------------------------------------
  [ ("countNodes of 2x + sin(y) is 6", countNodes sample == 6)
  , ("depth of 2x + sin(y) is 2", depth sample == 2)
  , ("a bare terminal has depth 0", depth (Var "x") == 0)
  , ("a bare terminal has one node", countNodes (Const 1.0) == 1)
  , ("flatten agrees with countNodes", length (flatten sample) == countNodes sample)
  , ("flatten is pre-order", flatten sample ==
      [ sample
      , Mul (Const 2.0) (Var "x")
      , Const 2.0
      , Var "x"
      , Sin (Var "y")
      , Var "y"
      ])
  , ("nodeDepths are per-node distances from the root",
      nodeDepths sample == [0, 1, 2, 2, 1, 2])
  , ("nodeDepths agrees with countNodes",
      length (nodeDepths sample) == countNodes sample)
  , ("variablesOf finds each variable once", variablesOf sample == ["x", "y"])
  , ("variablesOf deduplicates",
      variablesOf (Add (Var "x") (Mul (Var "x") (Var "x"))) == ["x"])
  , ("arity of a binary node is 2", arity sample == 2)
  , ("arity of a unary node is 1", arity (Sin (Var "y")) == 1)
  , ("arity of a terminal is 0", arity (Var "y") == 0)
  , ("isTerminal identifies leaves", isTerminal (Const 3.0) && isTerminal (Var "q"))
  , ("isTerminal rejects internal nodes", not (isTerminal sample))
  , ("opName ignores terminal payloads", opName (Const 1.0) == opName (Const 99.0))
  , ("opName distinguishes operators", opName (Add (Const 1.0) (Const 1.0)) == "+"
      && opName (Sin (Const 1.0)) == "sin")

  -- Addressing ------------------------------------------------------------
  , ("subtreeAt 0 is the whole tree", subtreeAt 0 sample == Just sample)
  , ("subtreeAt 2 is the constant", subtreeAt 2 sample == Just (Const 2.0))
  , ("subtreeAt 4 is the sin node", subtreeAt 4 sample == Just (Sin (Var "y")))
  , ("subtreeAt past the end is Nothing", subtreeAt 6 sample == Nothing)
  , ("subtreeAt a negative index is Nothing", subtreeAt (-1) sample == Nothing)
  , ("replaceAt 0 replaces everything",
      replaceAt 0 (Const 9.0) sample == Just (Const 9.0))
  , ("replaceAt 2 replaces the constant",
      replaceAt 2 (Const 5.0) sample
        == Just (Add (Mul (Const 5.0) (Var "x")) (Sin (Var "y"))))
  , ("replaceAt 5 replaces inside the right branch",
      replaceAt 5 (Var "z") sample
        == Just (Add (Mul (Const 2.0) (Var "x")) (Sin (Var "z"))))
  , ("replaceAt past the end is Nothing",
      replaceAt 6 (Const 1.0) sample == Nothing)
  , ("replaceAt a negative index is Nothing",
      replaceAt (-1) (Const 1.0) sample == Nothing)
  , ("replaceAt with the original subtree is the identity",
      all identityAt [0 .. countNodes sample - 1])

  -- Evaluation ------------------------------------------------------------
  , ("2x + sin(y) at x=3, y=0 is 6",
      approx (eval [("x", 3.0), ("y", 0.0)] sample) 6.0)
  , ("2x + sin(y) at x=0, y=0 is 0",
      approx (eval [("x", 0.0), ("y", 0.0)] sample) 0.0)
  , ("2x + sin(y) at x=-1.5, y=pi/2 is -2",
      approx (eval [("x", -1.5), ("y", pi / 2)] sample) (-2.0))
  , ("subtraction evaluates left-associatively",
      approx (eval [] (Sub (Sub (Const 10.0) (Const 3.0)) (Const 2.0))) 5.0)
  , ("cos(0) is 1", approx (eval [] (Cos (Const 0.0))) 1.0)
  , ("an unbound variable reads as 0", approx (eval [] (Var "nope")) 0.0)
  , ("an unused binding is ignored",
      approx (eval [("z", 99.0), ("x", 2.0), ("y", 0.0)] sample) 4.0)

  -- Protected operations --------------------------------------------------
  , ("protected division by a literal zero yields 1",
      approx (eval [] (Div (Const 1.0) (Const 0.0))) 1.0)
  , ("protected division by a computed zero yields 1",
      approx (eval [("x", 5.0)] (Div (Const 7.0) (Sub (Var "x") (Var "x")))) 1.0)
  , ("protected division by a tiny denominator yields 1",
      approx (protectedDiv 1.0 1.0e-15) 1.0)
  , ("protected division is ordinary division otherwise",
      approx (protectedDiv 6.0 3.0) 2.0)
  , ("0/0 does not produce NaN", not (isNaN (eval [] (Div (Const 0.0) (Const 0.0)))))
  , ("evaluation never yields NaN for a nested division",
      not (isNaN (eval [] (Div (Div (Const 1.0) (Const 0.0)) (Const 0.0)))))
  , ("protected log of zero is 0", approx (protectedLog 0.0) 0.0)
  , ("protected log of a negative uses the magnitude",
      approx (protectedLog (negate (exp 1.0))) 1.0)
  , ("protected sqrt of a negative uses the magnitude",
      approx (protectedSqrt (-4.0)) 2.0)
  , ("sanitize maps NaN to 0", approx (sanitize notANumber) 0.0)
  , ("sanitize clamps positive infinity",
      approx (sanitize positiveInfinity) magnitudeCap)
  , ("sanitize clamps negative infinity",
      approx (sanitize (negate positiveInfinity)) (negate magnitudeCap))
  , ("sanitize clamps oversized finite values",
      approx (sanitize 1.0e300) magnitudeCap)
  , ("sanitize leaves ordinary values alone", approx (sanitize 3.25) 3.25)

  -- Checked evaluation ----------------------------------------------------
  , ("evalChecked reports an unbound variable",
      evalChecked [] (Var "x") == Left "unbound variable: x")
  , ("evalChecked agrees with eval when bound",
      evalChecked [("x", 2.0)] (Mul (Var "x") (Const 3.0)) == Right 6.0)
  , ("evalChecked protects division too",
      evalChecked [] (Div (Const 1.0) (Const 0.0)) == Right 1.0)
  , ("evalChecked propagates the first error",
      evalChecked [("x", 1.0)] (Add (Var "x") (Var "w"))
        == Left "unbound variable: w")

  -- Pretty printing -------------------------------------------------------
  , ("pretty renders the benchmark formula", pretty sample == "2 * x + sin(y)")

  -- Powers of a possibly-negative base ----------------------------------------
    -- 'protectedPow' takes the magnitude of a negative base, so this really
    -- does compute |sin x| ^ 1.1 — a rectified sine. Rendered as written it
    -- looks like it should be complex-valued, and a reader would discard a
    -- legitimate discovery as broken output. Taken from a real `mixed`
    -- front individual.
  , ("a non-integer power of a sign-varying base shows the absolute value",
      pretty (Pow (Sin (Var "x")) (Const 1.1)) == "abs(sin(x)) ^ 1.1")
    -- No abs where it would be noise: these bases cannot be negative.
  , ("a non-negative base is left alone",
      pretty (Pow (Abs (Var "x")) (Const 1.1)) == "abs(x) ^ 1.1")
  , ("a squared base is left alone",
      pretty (Pow (Mul (Var "x") (Var "x")) (Const 0.5)) == "(x * x) ^ 0.5")
  , ("a positive constant base is left alone",
      pretty (Pow (Const 2.0) (Const 1.1)) == "2 ^ 1.1")
    -- The exponent is irrelevant: the magnitude is taken whatever it is,
    -- so the rendering depends only on the base. An earlier version also
    -- required a literal non-integer exponent, which would have left these
    -- two printing without the absolute value while computing with it.
  , ("an integer exponent still shows the absolute value",
      pretty (Pow (Sin (Var "x")) (Const 3.0)) == "abs(sin(x)) ^ 3")
  , ("a variable exponent still shows the absolute value",
      pretty (Pow (Const (-0.6)) (Var "y")) == "abs(-0.6) ^ y")

  -- The predicate behind it ---------------------------------------------------
    -- One-sided: True must be correct, False only means "not proven".
  , ("abs, exp, sqrt and non-negative constants are provably non-negative",
      all provablyNonNegative
        [Abs (Var "x"), Exp (Var "x"), Sqrt (Var "x"), Const 0.0, Const 3.0])
  , ("squaring is recognised", provablyNonNegative (Mul (Var "x") (Var "x")))
  , ("multiplying two different things is not",
      not (provablyNonNegative (Mul (Var "x") (Var "y"))))
  , ("an even integer power is non-negative whatever the base",
      provablyNonNegative (Pow (Var "x") (Const 2.0)))
  , ("an odd integer power is not",
      not (provablyNonNegative (Pow (Var "x") (Const 3.0))))
  , ("a bare variable is not provably non-negative",
      not (provablyNonNegative (Var "x")))
  , ("a negative constant is not", not (provablyNonNegative (Const (-1.0))))
  , ("non-integer constants are detected",
      map isNonIntegerConst [Const 1.1, Const 2.0, Const (-0.5), Var "x"]
        == [True, False, True, False])
  , ("pretty brackets a lower-precedence left operand",
      pretty (Mul (Add (Var "x") (Var "y")) (Const 3.0)) == "(x + y) * 3")
  , ("pretty omits brackets for left-associated subtraction",
      pretty (Sub (Sub (Var "x") (Var "y")) (Var "z")) == "x - y - z")
  , ("pretty brackets a right-nested subtraction",
      pretty (Sub (Var "x") (Sub (Var "y") (Var "z"))) == "x - (y - z)")
  , ("pretty omits brackets for a multiply inside an add",
      pretty (Add (Var "x") (Mul (Var "y") (Var "z"))) == "x + y * z")
  , ("pretty brackets a negative constant in an operator context",
      pretty (Mul (Const 2.0) (Const (-1.0))) == "2 * (-1)")
  , ("pretty leaves a bare negative constant unbracketed",
      pretty (Const (-1.5)) == "-1.5")
  , ("pretty trims trailing zeros", pretty (Const 0.5) == "0.5")
  , ("pretty renders whole numbers without a point", pretty (Const 100.0) == "100")
  , ("pretty renders nested calls", pretty (Sin (Cos (Var "x"))) == "sin(cos(x))")
  , ("pretty round-trips through a structural change",
      pretty (Add (Const 2.0) (Var "x")) /= pretty (Add (Var "x") (Const 2.0)))

  -- Exponentiation ----------------------------------------------------------
  , ("x^2 squares", approx (eval [("x", 3.0)] (Pow (Var "x") (Const 2.0))) 9.0)
  , ("x^3 cubes", approx (eval [("x", 2.0)] (Pow (Var "x") (Const 3.0))) 8.0)
  , ("x^0.5 is a square root",
      approx (eval [("x", 16.0)] (Pow (Var "x") (Const 0.5))) 4.0)
  , ("x^a handles a symbolic exponent",
      approx (eval [("x", 2.0), ("a", 10.0)] (Pow (Var "x") (Var "a"))) 1024.0)
  , ("a negative exponent reciprocates",
      approx (eval [("x", 4.0)] (Pow (Var "x") (Const (-1.0)))) 0.25)
    -- The four domain quadrants of real exponentiation, which is where an
    -- unprotected Pow would hand back NaN.
  , ("anything to the power 0 is 1",
      all (\b -> protectedPow b 0.0 == 1.0) [-5.0, -0.5, 0.0, 0.5, 5.0])
  , ("zero to a positive power is 0", approx (protectedPow 0.0 3.0) 0.0)
  , ("zero to a negative power hits the pole sentinel",
      protectedPow 0.0 (-2.0) == 1.0)
    -- Pow means |base| ** expo for every base. See
    -- docs/phase5-pow-semantics.md.
  , ("a negative base with an even integer exponent is positive",
      approx (protectedPow (-2.0) 2.0) 4.0)
    -- The named cost of that decision, asserted rather than left implicit:
    -- (-2)^3 is +8 here, not -8. An odd integer power of a negative base is
    -- well defined mathematically and this operator can no longer spell it.
    -- The shape stays reachable as Mul x (Mul x x), which is exact and
    -- continuous, so what is lost is a spelling rather than a function.
    -- If a benchmark with a genuine odd power regresses, start here.
  , ("an odd integer exponent no longer keeps the sign",
      approx (protectedPow (-2.0) 3.0) 8.0)
  , ("a negative base with a fractional exponent uses the magnitude",
      approx (protectedPow (-4.0) 0.5) 2.0)
    -- The point of the change: the old implementation kept the sign for
    -- integer exponents and dropped it otherwise, so the result flipped
    -- sign at every integer. Sampling either side of one is what would
    -- catch a regression to that behaviour.
  , ("the sign does not flip across an integer exponent",
      all (\e -> protectedPow (-0.6) e > 0.0) [2.0, 2.5, 3.0, 3.5, 4.0])
  , ("the operator is continuous across an integer exponent",
      abs (protectedPow (-0.6) 3.0 - protectedPow (-0.6) 3.0001) < 1.0e-3)
  , ("a negative base agrees with its magnitude everywhere",
      all (\(b, e) -> protectedPow b e == protectedPow (abs b) e)
          [(-2.0, 3.0), (-0.6, 2.0), (-4.0, 0.5), (-1.5, 7.0), (-9.0, 1.25)])
  , ("exponentiation saturates rather than overflowing",
      protectedPow 10.0 400.0 == magnitudeCap)
  , ("exponentiation never returns NaN across the plane",
      not (any isNaN
             [ protectedPow b e
             | b <- [-1.0e12, -7.5, -2.0, -0.5, 0.0, 0.5, 2.0, 1.0e12]
             , e <- [-1.0e6, -2.5, -1.0, 0.0, 0.5, 1.0, 2.0, 3.0, 1.0e6]
             ]))
    -- Bases here are wrapped in 'Abs' so these stay tests of /bracketing/.
    -- A bare variable base now renders its own absolute value, which is
    -- correct but would make every expected string below a test of two
    -- things at once. The absolute-value rendering has its own tests above.
  , ("pow prints as an infix caret",
      pretty (Pow (Abs (Var "x")) (Const 2.0)) == "abs(x) ^ 2")
  , ("pow binds tighter than multiplication",
      pretty (Mul (Pow (Abs (Var "x")) (Const 2.0)) (Var "y")) == "abs(x) ^ 2 * y")
  , ("a sum in the base is bracketed",
      pretty (Pow (Add (Var "x") (Var "y")) (Const 2.0)) == "abs(x + y) ^ 2")
  , ("pow associates to the right without brackets",
      pretty (Pow (Abs (Var "x")) (Pow (Abs (Var "y")) (Var "z")))
        == "abs(x) ^ abs(y) ^ z")
  , ("a left-nested pow is bracketed",
      pretty (Pow (Pow (Abs (Var "x")) (Var "y")) (Var "z"))
        == "(abs(x) ^ y) ^ z")

  -- Special functions: elementary ------------------------------------------
  , ("exp(0) is 1", approx (eval [] (Exp (Const 0.0))) 1.0)
  , ("exp(1) is e", approx (eval [] (Exp (Const 1.0))) (exp 1.0))
  , ("exp saturates instead of overflowing",
      eval [] (Exp (Const 10000.0)) == magnitudeCap)
  , ("exp of a large negative underflows to 0",
      approx (eval [] (Exp (Const (-10000.0)))) 0.0)
  , ("protectedExp agrees with exp in range", approx (protectedExp 2.0) (exp 2.0))
  , ("log(e) is 1", approx (eval [] (Log (Const (exp 1.0)))) 1.0)
  , ("log uses the protected form at zero",
      approx (eval [] (Log (Const 0.0))) 0.0)
  , ("log uses the magnitude of a negative",
      approx (eval [] (Log (Const (negate (exp 1.0))))) 1.0)
  , ("sqrt(9) is 3", approx (eval [] (Sqrt (Const 9.0))) 3.0)
  , ("sqrt uses the magnitude of a negative",
      approx (eval [] (Sqrt (Const (-16.0)))) 4.0)
  , ("tanh(0) is 0", approx (eval [] (Tanh (Const 0.0))) 0.0)
  , ("tanh saturates towards 1", approx (eval [] (Tanh (Const 50.0))) 1.0)
  , ("tanh is odd",
      approx (eval [] (Tanh (Const (-1.5)))) (negate (eval [] (Tanh (Const 1.5)))))
  , ("abs of a negative is positive", approx (eval [] (Abs (Const (-3.5)))) 3.5)
  , ("abs of a positive is unchanged", approx (eval [] (Abs (Const 3.5))) 3.5)

  -- Gamma -------------------------------------------------------------------
    -- Lanczos approximation; these are the standard checkpoints. gamma(n)
    -- is (n-1)! for positive integers, and gamma(1/2) is sqrt(pi).
  , ("gamma(1) is 1", closeTo (eval [] (Gamma (Const 1.0))) 1.0 1.0e-9)
  , ("gamma(2) is 1", closeTo (eval [] (Gamma (Const 2.0))) 1.0 1.0e-9)
  , ("gamma(5) is 24", closeTo (eval [] (Gamma (Const 5.0))) 24.0 1.0e-7)
  , ("gamma(6) is 120", closeTo (eval [] (Gamma (Const 6.0))) 120.0 1.0e-6)
  , ("gamma(0.5) is sqrt(pi)",
      closeTo (eval [] (Gamma (Const 0.5))) (sqrt pi) 1.0e-9)
  , ("gamma(1.5) is sqrt(pi)/2",
      closeTo (eval [] (Gamma (Const 1.5))) (sqrt pi / 2.0) 1.0e-9)
    -- Reflection formula territory.
  , ("gamma(-0.5) is -2 sqrt(pi)",
      closeTo (eval [] (Gamma (Const (-0.5)))) (negate (2.0 * sqrt pi)) 1.0e-8)
  , ("gamma satisfies its own recurrence",
      closeTo (gammaFunction 4.3) (3.3 * gammaFunction 3.3) 1.0e-8)
  , ("gamma at 0 hits the pole sentinel", protectedGamma 0.0 == 1.0)
  , ("gamma at -1 hits the pole sentinel", protectedGamma (-1.0) == 1.0)
  , ("gamma at -7 hits the pole sentinel", protectedGamma (-7.0) == 1.0)
  , ("gamma just off a pole does not", protectedGamma (-1.5) /= 1.0)
  , ("gamma of a large argument saturates rather than overflowing",
      eval [] (Gamma (Const 500.0)) == magnitudeCap)
  , ("gamma never returns NaN near its poles",
      not (any (isNaN . protectedGamma) [-5.0, -4.999, -3.0, -0.0001, 0.0, 1.0e-8]))

  -- Zeta --------------------------------------------------------------------
    -- Euler-Maclaurin. The Basel problem and its cousin are the standard
    -- checkpoints above the line of convergence; zeta(0) and zeta(-1) test
    -- that the same formula really is performing analytic continuation,
    -- and zeta(-2) that the trivial zeros land where they should.
  , ("zeta(2) is pi^2/6",
      closeTo (eval [] (Zeta (Const 2.0))) (pi * pi / 6.0) 1.0e-9)
  , ("zeta(4) is pi^4/90",
      closeTo (eval [] (Zeta (Const 4.0))) (pi ** 4.0 / 90.0) 1.0e-9)
  , ("zeta(6) is pi^6/945",
      closeTo (eval [] (Zeta (Const 6.0))) (pi ** 6.0 / 945.0) 1.0e-9)
  , ("zeta(0) is -1/2", closeTo (eval [] (Zeta (Const 0.0))) (-0.5) 1.0e-9)
  , ("zeta(-1) is -1/12",
      closeTo (eval [] (Zeta (Const (-1.0)))) (-1.0 / 12.0) 1.0e-9)
  , ("zeta(-3) is 1/120",
      closeTo (eval [] (Zeta (Const (-3.0)))) (1.0 / 120.0) 1.0e-9)
  , ("zeta(-2) is a trivial zero",
      closeTo (eval [] (Zeta (Const (-2.0)))) 0.0 1.0e-9)
  , ("zeta(-4) is a trivial zero",
      closeTo (eval [] (Zeta (Const (-4.0)))) 0.0 1.0e-8)
  , ("zeta tends to 1 for large s",
      closeTo (eval [] (Zeta (Const 40.0))) 1.0 1.0e-9)
  , ("zeta at 1 hits the pole sentinel", protectedZeta 1.0 == 1.0)
  , ("zeta just above 1 is large and finite",
      let v = protectedZeta 1.0001
      in v > 100.0 && v < magnitudeCap)
    -- Below -7 the reflection formula takes over, so this checks that the
    -- second branch agrees with the first. Negative odd integers satisfy
    -- zeta(-n) = -B(n+1)/(n+1), so zeta(-9) = -B(10)/10 = -(5/66)/10.
  , ("zeta below the continuation floor uses reflection",
      closeTo (protectedZeta (-9.0)) (negate (1.0 / 132.0)) 1.0e-6)
  , ("zeta(-5) is -1/252",
      closeTo (protectedZeta (-5.0)) (negate (1.0 / 252.0)) 1.0e-9)
  , ("the two zeta branches agree across the changeover",
      closeTo (protectedZeta (-6.999)) (protectedZeta (-7.001)) 1.0e-3)
  , ("zeta never returns NaN across a wide sweep",
      not (any (isNaN . protectedZeta)
             [-200.0, -50.0, -9.0, -2.0, -1.0, 0.0, 0.5, 1.5, 2.0, 100.0]))
  , ("the raw Euler-Maclaurin kernel agrees with the protected wrapper",
      closeTo (zetaEulerMaclaurin 3.0) (protectedZeta 3.0) 1.0e-12)

  -- Continuity across the algorithm-selection boundaries -------------------
    -- 'protectedZeta' switches implementation at s = -7 (Euler-Maclaurin
    -- above, functional-equation reflection below) and again at s = -160.
    -- Neither is a pole: zeta is perfectly smooth at both points, so the
    -- boundaries are computational, not mathematical.
    --
    -- That makes them the one place in the operator set where the pattern
    -- that produced two bugs already — a branch that is locally defensible
    -- and globally discontinuous — could hide without any mathematical
    -- excuse. Audited explicitly before writing derivative rules that
    -- assume continuity.
    --
    -- __Measured, 2026-08-19: both boundaries are discontinuous, and they
    -- are discontinuous for entirely different reasons.__
    --
    -- At s = -7 the two implementations disagree by between 1e-6 and 1e-4
    -- on a function whose value there is about 1/240. That is an /accuracy
    -- seam/ — two approximations of the same smooth function meeting
    -- imperfectly — not a semantic branch. It is bounded here rather than
    -- asserted away, so a refactor that widened it would fail.
  , ("the seam at the continuation floor stays within 1e-4",
      closeTo (protectedZeta (-6.999)) (protectedZeta (-7.001)) 1.0e-4)
  , ("...and is not merely absent", protectedZeta (-6.999) /= protectedZeta (-7.001))
  , ("zeta near -7 is about -B_8/8 = 1/240",
      closeTo (protectedZeta (-7.001)) 0.0041666 1.0e-3)
    -- At s = -160 the two sides differ by 2 * magnitudeCap, which looks
    -- alarming and is not a branch artefact at all. __Both__ sides
    -- saturate: zeta genuinely exceeds the cap there, since zeta(-159) is
    -- -B_160/160 and B_160 is around 1e199. What differs is the sign, and
    -- the sign difference is real mathematics — s = -160 is a trivial
    -- zero, and zeta changes sign at every even negative integer.
    --
    -- So the jump is saturation destroying a genuine zero crossing, not an
    -- implementation seam. 'evalDomain' already reports both sides as
    -- 'Saturated', which is the correct account of what happened.
  , ("both sides of the reflection floor saturate",
      abs (protectedZeta (-159.999)) == magnitudeCap
        && abs (protectedZeta (-160.001)) == magnitudeCap)
  , ("and they carry opposite signs, because -160 is a trivial zero",
      signum (protectedZeta (-159.999)) /= signum (protectedZeta (-160.001)))

  -- Structure and printing of the new nodes ---------------------------------
  , ("new operators are unary", all ((== 1) . arity)
      [ Exp (Var "x"), Log (Var "x"), Sqrt (Var "x")
      , Tanh (Var "x"), Abs (Var "x"), Gamma (Var "x"), Zeta (Var "x")
      ])
  , ("new operators report their names",
      map opName [Exp (Var "x"), Log (Var "x"), Sqrt (Var "x"), Tanh (Var "x")
                 , Abs (Var "x"), Gamma (Var "x"), Zeta (Var "x")]
        == ["exp", "log", "sqrt", "tanh", "abs", "gamma", "zeta"])
  , ("new operators print as calls",
      pretty (Gamma (Add (Var "x") (Const 1.0))) == "gamma(x + 1)")
  , ("nested new operators print",
      pretty (Zeta (Sqrt (Var "x"))) == "zeta(sqrt(x))")
  , ("new operators participate in addressing",
      subtreeAt 1 (Gamma (Var "y")) == Just (Var "y"))
  , ("new operators participate in replacement",
      replaceAt 1 (Const 2.0) (Tanh (Var "y")) == Just (Tanh (Const 2.0)))
  , ("new operators count and measure correctly",
      countNodes (Gamma (Zeta (Var "x"))) == 3
        && depth (Gamma (Zeta (Var "x"))) == 2)

  -- Totality across every operator -----------------------------------------
    -- The engine's central invariant: no operator, at any input, may hand
    -- a caller a non-finite value.
  , ("no unary operator produces a non-finite result at any probe point",
      all finite
        [ eval [("x", v)] (build (Var "x"))
        | v <- probePoints
        , build <- [Sin, Cos, Exp, Log, Sqrt, Tanh, Abs, Gamma, Zeta]
        ])
  , ("no unary operator produces a non-finite result when nested",
      all finite
        [ eval [("x", v)] (outer (inner (Var "x")))
        | v <- probePoints
        , outer <- [Exp, Log, Sqrt, Gamma, Zeta]
        , inner <- [Exp, Log, Sqrt, Gamma, Zeta]
        ])
  ]
  where
    -- Replacing a subtree with itself must leave the tree untouched. This
    -- exercises the index arithmetic in replaceAt at every position at
    -- once, which is where an off-by-one in the left/right split would
    -- show up.
    identityAt :: Int -> Bool
    identityAt i = case subtreeAt i sample of
      Nothing  -> False
      Just sub -> replaceAt i sub sample == Just sample

    closeTo :: Double -> Double -> Double -> Bool
    closeTo actual expected tolerance =
      abs (actual - expected) <= tolerance * (1.0 + abs expected)

    finite :: Double -> Bool
    finite v = not (isNaN v) && not (isInfinite v)

    -- Deliberately includes the awkward cases: zero, negatives, the poles
    -- of gamma, the pole of zeta, and magnitudes near the clamp.
    probePoints :: [Double]
    probePoints =
      [ -1.0e12, -1000.0, -170.5, -7.0, -3.0, -2.0, -1.0, -0.5
      , 0.0, 1.0e-12, 0.5, 1.0, 1.5, 2.0, 3.0, 171.0, 1000.0, 1.0e12
      ]
