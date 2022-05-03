(* Isabelle/HOL file used to generate pe*.p. *)

theory PE
  imports Main
begin

(* 1. SPE should eliminate p *)
lemma
  assumes
    "u = v \<or> p x"
    "p (f x) \<or> w = x"
    "y = z"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 2. SPE should eliminate p *)
lemma
  assumes
    "p one \<or> f one = two"
    "p two \<or> f two = three"
    "p three \<or> f three = four"
    "\<not> p one \<or> f four = five"
    "\<not> p two \<or> f five = six"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 3. SPE should eliminate p and q *)
lemma
  assumes
    "p one \<or> f one = two"
    "p two \<or> f two = three"
    "p three \<or> f three = four"
    "\<not> p one \<or> f four = five"
    "\<not> p two \<or> f five = six"
    "q one \<or> f one = two"
    "q two \<or> f two = three"
    "q three \<or> f three = four"
    "\<not> q one \<or> f four = five"
    "\<not> q two \<or> f five = six"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 4. SPE should not eliminate any predicate *)
lemma
  assumes
    "p one \<or> \<not> p two"
    "p two \<or> f two = three"
    "p three \<or> f three = four"
    "\<not> p one \<or> f four = five"
    "\<not> p two \<or> f five = six"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops


end
