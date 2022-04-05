(* Isabelle/HOL file used to generate qle*.p. *)

theory QLE
  imports Main
begin

(* p and q are quasipure *)
lemma
  assumes
    "p x \<or> q a x"
    "p (f x)"
    "\<not> q a a"
  shows False
  oops

(* p is quasipure *)
lemma
  assumes
    "p a"
    "\<not> p x \<or> p (f x)"
  shows False
  oops

(* p and q are quasipure *)
lemma
  assumes
    "p a"
    "q x \<or> p (f x)"
    "\<not> q (f a)"
    "\<not> p x \<or> \<not> q (h p (p b))"
  shows False
  oops

(* \<emptyset> is quasipure *)
lemma
  assumes
    "p a"
    "q p"
    "\<not> q p"
    "r"
  shows False
  oops

(* \<emptyset> is quasipure *)
lemma
  assumes
    "p a"
    "p = q"
    "\<not> (p = q)"
    "r"
  shows False
  sledgehammer [zipperposition, overlord, dont_slice] (assms)
  oops

end
