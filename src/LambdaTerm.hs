module LambdaTerm (
    Term (
        Variable,
        Abst,
        App
    ),
    Context,
    showTerm,
) where

--
-- A representation of a lambda term.
--
data Term
    = Variable String   -- x
    | Abst String Term  -- λx. M
    | App Term Term     -- M M
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

showTermWithParen term = "("++ showTerm term ++ ")"

--
-- Formats a lambda term in a human-friendly way.
--
showTerm :: Term -> String

showTerm (Variable name) = name

showTerm (Abst name term) = "λ" ++ name ++ ". " ++ showTerm term

showTerm (App (App term1 term2) term3) = showTerm (App term1 term2) ++ " " ++ showTermWithParen term3

showTerm (App term1 term2) = showTermWithParen term1 ++ " " ++ showTermWithParen term2
