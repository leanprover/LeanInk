namespace LeanInk.CLI

-- Result
inductive Result (error : Type) (result: Type) where
  | failure (err: error)
  | success (res: result)

instance [ToString e] : ToString (Result e r) where
  toString
    | Result.failure error => s!"ERROR: {error}"
    | Result.success result => "SUCCESS!"