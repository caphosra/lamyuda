module LambdaTerm (
    Term (
        Variable,
        Abst,
        App
    ),
    Context,
    showTerm,
    showTermScheme,
    showTermPython,
) where

--
-- A representation of a lambda term.
--
data Term
    = Variable String -- x
    | Abst String Term -- λx. M
    | App Term Term -- M M
    deriving (Eq, Show)

--
-- A representation of a context.
--
type Context = [(String, Term)]

--
-- Wraps a term with parentheses if it is not a variable.
--
showTermWithParen :: Term -> String
showTermWithParen (Variable name) = name
showTermWithParen term = "(" ++ showTerm term ++ ")"

--
-- Formats a lambda term in a human-friendly way.
--
showTerm :: Term -> String
showTerm (Variable name) = name
showTerm (Abst name term) = "λ" ++ name ++ ". " ++ showTerm term
showTerm (App (App term1 term2) term3) = showTerm (App term1 term2) ++ " " ++ showTermWithParen term3
showTerm (App term1 term2) = showTermWithParen term1 ++ " " ++ showTermWithParen term2

--
-- Formats a lambda term in Scheme style.
--
showTermScheme :: Term -> String
showTermScheme term = case term of
    Variable name -> replacePrime name
    Abst name term' -> "(lambda (" ++ replacePrime name ++ ") " ++ showTermScheme term' ++ ")"
    App term1 term2 -> "(" ++ showTermScheme term1 ++ " " ++ showTermScheme term2 ++ ")"
  where
    replacePrime = map (\x -> if x == '\'' then '_' else x)

--
-- Formats a lambda term in Python style.
--
showTermPython :: Term -> String
showTermPython term = case term of
    Variable name -> replacePrime name
    Abst name term' -> "lambda " ++ replacePrime name ++ ": " ++ showTermPython term'
    App term1 term2 -> showTermPythonWithParen term1 ++ " (" ++ showTermPython term2 ++ ")"
  where
    replacePrime = map (\x -> if x == '\'' then '_' else x)
    showTermPythonWithParen (Variable name) = replacePrime name
    showTermPythonWithParen term' = "(" ++ showTermPython term' ++ ")"
