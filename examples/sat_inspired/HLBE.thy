(* Isabelle/HOL file used to generate hlbe*.p. *)

theory HLBE
  imports Main
begin

(* HLE should simplify "cyan \<or> blue \<or> yellow" to "blue \<or> yellow" *)
lemma
  assumes
    "cyan \<longrightarrow> blue"
    "cyan \<longrightarrow> green"
    "cyan \<or> blue \<or> yellow"
  shows False
  oops

(* FLE should simplify "blue \<or> yellow" to "yellow", but somehow this is handled
   by UnitHLE? *)
lemma
  assumes
    "blue \<longrightarrow> cyan"
    "blue \<longrightarrow> \<not> cyan"
    "blue \<or> yellow"
  shows False
  sledgehammer [zipperposition, overlord, dont_slice] (assms)
  oops

end
