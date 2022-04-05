(* Isabelle/HOL file used to generate qle*.p. *)

theory QLE
  imports Main
begin

(* 1. p and q are quasipure *)
lemma
  assumes
    "p x \<or> q a x"
    "p (f x)"
    "\<not> q a a"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 2. p is quasipure *)
lemma
  assumes
    "p a"
    "\<not> p x \<or> p (f x)"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 3. p and q are quasipure *)
lemma
  assumes
    "p a"
    "q x \<or> p (f x)"
    "\<not> q (f a)"
    "\<not> p x \<or> \<not> q (h p (p b))"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 4. r is quasipure *)
lemma
  assumes
    "p a"
    "q p"
    "\<not> q p"
    "r"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 5. r is quasipure *)
lemma
  assumes
    "p a"
    "p = q"
    "\<not> (p = q)"
    "r"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* Same again, but this time polymorphic *)

sledgehammer_params[type_enc = poly_native_higher_fool]

axiomatization
  a :: 'a and
  p :: "'a \<Rightarrow> bool" and
  q :: "'a \<Rightarrow> 'b" and
  r :: bool

(* 6. p and q are quasipure *)
lemma assms_6:
  "p x \<or> q a x"
  "p (f x)"
  "\<not> q a a"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_6) *)
  oops

(* 7. p is quasipure *)
lemma assms_7:
  "p a"
  "\<not> p x \<or> p (f x)"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_7) *)
  oops

(* 8. p and q are quasipure *)
lemma assms_8:
  "p a"
  "q x \<or> p (f x)"
  "\<not> q (f a)"
  "\<not> p x \<or> \<not> q (h p (p b))"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_8) *)
  oops

(* 9. r is quasipure *)
lemma assms_9:
  "p a"
  "q p"
  "\<not> q p"
  "r"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_9) *)
  oops

(* 10. r is quasipure *)
lemma assms_10:
  "p a"
  "p = q"
  "\<not> (p = q)"
  "r"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_10) *)
  oops

end
