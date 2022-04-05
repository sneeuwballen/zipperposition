(* Isabelle/HOL file used to generate pe*.p. *)

theory PE
  imports Main
begin

(* 1. SPE should eliminate p *)
lemma
  assumes
    "q a \<or> p x"
    "p (f x) \<or> r b"
  shows False
  sledgehammer [zipperposition, overlord, dont_slice] (assms)
  oops

(*
\<not> p x \<or> q x
\<not> p x \<or> r x \<or> s x
p x \<or> \<not> q x \<or> \<not> r x
p x \<or> \<not> q x \<or> \<not> s x
*)

end
