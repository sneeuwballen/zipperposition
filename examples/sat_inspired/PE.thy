(* Isabelle/HOL file used to generate pe*.p. *)

theory PE
  imports Main
begin

(* 1. SPE should eliminate p *)
lemma
  assumes
    "q u \<or> q v \<or> p x"
    "\<not> p x \<or> q w"
    "q x"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 2. SPE should eliminate p *)
lemma
  assumes
    "q u \<or> q v \<or> p x"
    "\<not> p (f x) \<or> q w"
    "q x"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 3. SPE should eliminate p *)
lemma
  assumes
    "q u \<or> q v \<or> p x"
    "\<not> p (f x) \<or> q w"
    "x = y"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 4. SPE should eliminate p *)
lemma
  assumes
    "\<And>x. q u \<or> q v \<or> p x"
    "\<not> p (f x) \<or> q w"
    "x = y"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 5. SPE should eliminate p *)
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

(* 6. SPE should eliminate p and q *)
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

(* 7. SPE should not eliminate any predicate *)
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

(* The same, but this time polymorphic *)

sledgehammer_params[type_enc = poly_native_higher_fool]

axiomatization
  f :: "'a \<Rightarrow> 'a" and
  p :: "'a \<Rightarrow> bool" and
  q :: "'a \<Rightarrow> bool" and
  u v w x y z :: 'a

(* 8. SPE should eliminate p *)
lemma assms_8:
  "q (u :: 'a) \<or> q (v :: 'a) \<or> p (x :: 'a \<times> int)"
  "\<not> p (x :: nat \<times> 'a) \<or> q (w :: 'a)"
  "q (x :: 'a)"
  "\<not> q (u :: unit)"
  "\<not> q (v :: unit)"
  "\<not> q (w :: int)"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_8) *)
  oops

(* 9. SPE should eliminate p *)
lemma assms_9:
  "q (u :: 'a) \<or> q (v :: 'a) \<or> p (x :: 'a \<times> int)"
  "\<not> p (f x :: nat \<times> 'a) \<or> q (w :: 'a)"
  "q (x :: 'a)"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_9) *)
  oops

(* 10. SPE should eliminate p *)
lemma assms_10:
  "q (u :: 'a) \<or> q (v :: 'a) \<or> p (x :: 'a \<times> int)"
  "\<not> p (f x :: nat \<times> 'a) \<or> q (w :: 'a)"
  "(x :: unit) = (y :: unit)"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_10) *)
  oops

(* 11. SPE should eliminate p *)
lemma assms_11:
  "\<And>x. q (u :: 'a) \<or> q (v :: 'a) \<or> p (x :: 'a \<times> int)"
  "\<not> p (f x :: nat \<times> 'a) \<or> q (w :: 'a)"
  "(x :: unit) = (y :: unit)"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_11) *)
  oops

(* FIXME: Make 12-14 polymorphic. *)

(* 12. SPE should eliminate p *)
lemma assms_12:
  "p (one :: 'a) \<or> f (one :: 'a) = (two :: 'a)"
  "p (two :: 'a) \<or> f (two :: 'a) = (three :: 'a)"
  "p three \<or> f three = four"
  "\<not> p one \<or> f four = five"
  "\<not> p two \<or> f five = six"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_12) *)
  oops

(* 13. SPE should eliminate p and q *)
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

(* 14. SPE should not eliminate any predicate *)
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

(* The same, but this time higher-order *)

(* 15. SPE should eliminate p *)
lemma
  fixes p q :: "('a \<Rightarrow> 'a) \<Rightarrow> bool"
  assumes
    "\<And>y. y u \<or> q u \<or> q v \<or> p (\<lambda>x. x)"
    "\<not> p (\<lambda>x. x) \<or> q w"
    "q x"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 16. SPE should eliminate p *)
lemma
  fixes p q :: "('a \<Rightarrow> 'a) \<Rightarrow> bool"
  assumes
    "\<And>y. y u \<or> q u \<or> q v \<or> p (\<lambda>x. x)"
    "\<not> p (\<lambda>x. f x) \<or> q w"
    "q x"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 17. SPE should eliminate p *)
lemma
  fixes p q :: "('a \<Rightarrow> 'a) \<Rightarrow> bool"
  assumes
    "\<And>y. y u \<or> q u \<or> q v \<or> p (\<lambda>x. x)"
    "\<not> p (\<lambda>x. f x) \<or> q w"
    "x = y"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 18. SPE should eliminate p *)
lemma
  fixes p q :: "('a \<Rightarrow> 'a) \<Rightarrow> bool"
  assumes
    "\<And>y x. y u \<or> q u \<or> q v \<or> p x"
    "\<not> p (\<lambda>x. f x) \<or> q w"
    "x = y"
  shows False
  sledgehammer [zipperposition, overlord, dont_slice] (assms)
  oops

(* 19. SPE should eliminate p *)
lemma
  fixes p q :: "('a \<Rightarrow> 'a) \<Rightarrow> bool"
  assumes
    "p one \<or> f one = two"
    "p two \<or> f two = three"
    "p three \<or> f three = four"
    "\<not> p one \<or> f four = five"
    "\<not> p two \<or> f five = six"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 20. SPE should eliminate p and q *)
lemma
  fixes p q :: "('a \<Rightarrow> 'a) \<Rightarrow> bool"
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

(* 21. SPE should not eliminate any predicate *)
lemma
  fixes p q :: "('a \<Rightarrow> 'a) \<Rightarrow> bool"
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
