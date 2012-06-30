module Loh.Players.Kludges where

eitherToMaybe ∷ Either α β → Maybe β
eitherToMaybe = either (const Nothing) Just

liftFstMaybe ∷ Monad μ ⇒ μ (μ α, β) → μ (α, β)
liftFstMaybe m = do
  (ma, b) ← m
  a ← ma
  return (a, b)
