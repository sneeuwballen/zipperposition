(* Isabelle/HOL file used to generate bce*.p. *)

theory BCE
  imports Main
begin

(* 1. both clauses are blocked *)
lemma
  assumes
    "\<not> p \<or> q"
    "p \<or> \<not> q"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 2. the first two clauses are blocked *)
lemma
  assumes
    "\<not> p \<or> q"
    "p \<or> \<not> q"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 3. the first two clauses are blocked *)
lemma
  assumes
    "\<not> p a \<or> q a"
    "p b \<or> \<not> q b"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 4. the first two clauses are blocked *)
lemma
  assumes
    "\<not> p a \<or> q a"
    "p b \<or> \<not> q b"
    "c = d"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 5. no clauses are blocked *)
lemma
  assumes
    "\<not> p a \<or> q a"
    "p b \<or> \<not> q c"
    "d = e"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 6. both clauses are blocked *)
lemma
  assumes
    "\<And>x. \<not> p x \<or> q x \<or> \<not> q a"
    "p a"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 7. neither clause is blocked *)
lemma
  assumes
    "p \<noteq> a \<or> q = a"
    "p = a \<or> q \<noteq> a"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 8. all three clauses are blocked *)
lemma
  assumes
    "\<not> p \<or> q"
    "p \<or> \<not> q"
    "\<not> r \<or> a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 9. neither clause is blocked *)
lemma
  assumes
    "\<And>x y. p x y \<or> p y x"
    "\<And>x y. \<not> p x y \<or> \<not> p y x"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 10. both clauses are blocked *)
lemma
  assumes
    "p a \<or> p b \<or> \<not> q"
    "\<And>x. \<not> p x \<or> q"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* The same, but this time polymorphic *)

sledgehammer_params [type_enc = poly_native_higher_fool]

axiomatization
  a b p q r :: 'a

(* 11. both clauses are blocked *)
lemma assms_11:
  "\<not> p \<or> q"
  "p \<or> \<not> q"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_11) *)
  oops

(* 12. the first two clauses are blocked *)
lemma assms_12:
  "\<not> p \<or> q"
  "p \<or> \<not> q"
  "a = b"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_12) *)
  oops

(* 13. the first two clauses are blocked *)
lemma assms_13:
  "\<not> p (a :: nat) \<or> q (a :: nat)"
  "p (b :: nat) \<or> \<not> q (b :: nat)"
  "(a :: nat) = (b :: nat)"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_13) *)
  oops

(* 14. the first two clauses are blocked *)
lemma assms_14:
  "\<not> p (a :: nat) \<or> q (a :: nat)"
  "p (b :: nat) \<or> \<not> q (b :: nat)"
  "c = d"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_14) *)
  oops

(* 15. no clauses are blocked *)
lemma assms_15:
  "\<not> p (a :: nat) \<or> q (a :: nat)"
  "p (b :: nat) \<or> \<not> q (c :: nat)"
  "d = e"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_15) *)
  oops

(* 16. both clauses are blocked *)
lemma assms_16:
  "\<And>x. \<not> p x \<or> q x \<or> \<not> q a"
  "p a"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_16) *)
  oops

(* 17. neither clause is blocked *)
lemma assms_17:
  "p \<noteq> (a :: nat) \<or> q = (a :: nat)"
  "p = (a :: nat) \<or> q \<noteq> (a :: nat)"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_17) *)
  oops

(* 18. all three clauses are blocked *)
lemma assms_18:
  "\<not> p \<or> q"
  "p \<or> \<not> q"
  "\<not> r \<or> a = b"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_18) *)
  oops

(* 19. neither clause is blocked *)
lemma assms_19:
  "\<And>(x :: nat) (y :: nat). p x y \<or> p y x"
  "\<And>(x :: nat) (y :: nat). \<not> p x y \<or> \<not> p y x"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_19) *)
  oops

(* 20. both clauses are blocked *)
lemma assms_20:
  "p (a :: nat) \<or> p (b :: nat) \<or> \<not> q"
  "\<And>x. \<not> p (x :: nat) \<or> q"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_20) *)
  oops

(* Same as 1–5, but this time higher-order *)

(* 21. both clauses are blocked *)
lemma
  fixes p q a
  assumes
    "\<And>y. \<not> p (\<lambda>x. x) \<or> q \<or> y a"
    "p (\<lambda>x. x) \<or> \<not> q"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 22. the first two clauses are blocked *)
lemma
  fixes p q a b
  assumes
    "\<And>y. \<not> p (\<lambda>x. x) \<or> q \<or> y a"
    "p (\<lambda>x. x) \<or> \<not> q"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 23. the first two clauses are blocked *)
lemma
  fixes p q a b
  assumes
    "\<And>y. \<not> p (\<lambda>x. x) a \<or> q a \<or> y a"
    "p (\<lambda>x. x) b \<or> \<not> q b"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 24. the first two clauses are blocked *)
lemma
  fixes p q a b c d
  assumes
    "\<And>y. \<not> p (\<lambda>x. x) a \<or> q a \<or> y a"
    "p (\<lambda>x. x) b \<or> \<not> q b"
    "c = d"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 25. no clauses are blocked *)
lemma
  fixes p q a b c d e
  assumes
    "\<And>y. \<not> p (\<lambda>x. x) a \<or> q a \<or> y a"
    "p (\<lambda>x. x) b \<or> \<not> q c"
    "d = e"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops


end
