(* Isabelle/HOL file used to generate dpe*.p. *)

theory DPE
  imports Main
begin

(* 1. DPE should eliminate p *)
lemma
  assumes
    "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
    "\<And>x y. p x y \<or> q x \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 2. DPE should eliminate p *)
lemma
  assumes
    "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
    "\<And>x y. p x y \<or> q x \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "p a b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 3. DPE should eliminate p *)
lemma
  assumes
    "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
    "\<And>x y. p x y \<or> q x \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 4. DPE should eliminate p *)
lemma
  assumes
    "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
    "\<And>x y. p x y \<or> q x \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "p a b"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 5. DPE should eliminate p *)
lemma
  assumes
    "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
    "\<And>x y. p x y \<or> q x \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "p a b \<or> \<not> p c d"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* The same, but this time polymorphic *)

sledgehammer_params [type_enc = poly_native_higher_fool]

axiomatization
  a b e :: 'a and
  p :: "'a \<Rightarrow> 'b \<Rightarrow> bool" and
  q :: "'a \<Rightarrow> 'a" and
  r :: "'a \<Rightarrow> 'a"

(* 6. DPE should eliminate p *)
lemma assms_6:
  "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
  "\<And>x y. p x y \<or> q x \<noteq> e"
  "\<And>x y. p x y \<or> r y \<noteq> e"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_6) *)
  oops

(* 7. DPE should eliminate p *)
lemma assms_7:
  "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
  "\<And>x y. p x y \<or> q x \<noteq> e"
  "\<And>x y. p x y \<or> r y \<noteq> e"
  "p (a :: 'a option) b"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_7) *)
  oops

(* 8. DPE should eliminate p *)
lemma assms_8:
  "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
  "\<And>x y. p x y \<or> q x \<noteq> e"
  "\<And>x y. p x y \<or> r y \<noteq> e"
  "a = b"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_8) *)
  oops

(* 9. DPE should eliminate p *)
lemma assms_9:
  "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
  "\<And>x y. p x y \<or> q x \<noteq> e"
  "\<And>x y. p x y \<or> r y \<noteq> e"
  "p (a :: 'a option) b"
  "a = b"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_9) *)
  oops

(* 10. DPE should eliminate p *)
lemma assms_10:
  "\<And>x y. \<not> p x y \<or> q x = e \<or> r y = e"
  "\<And>x y. p x y \<or> q x \<noteq> e"
  "\<And>x y. p x y \<or> r y \<noteq> e"
  "p (a :: 'a option) b \<or> \<not> p (c :: 'b \<times> 'c list) (d :: nat)"
  "a = b"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_10) *)
  oops

(* The same, but this time higher-order *)

sledgehammer_params [type_enc = mono_native_higher_fool]

(* 11. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 12. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "\<And>z. z e \<or> p (\<lambda>x. f x x) b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 13. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 14. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "\<And>z. z e \<or> p (\<lambda>x. f x x) b"
    "b = c"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 15. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "\<And>z. z e \<or> p (\<lambda>x. f x x) b \<or> \<not> p c d"
    "b = c"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* The same, but this time very higher-order, with deep and partial applications *)

(* 16. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
  shows "s p = e \<and> t (p a) = e \<and> u (p a c) = e"
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 17. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "\<And>z. z e \<or> p (\<lambda>x. f x x) b"
  shows "s p = e \<and> t (p a) = e \<and> u (p a c) = e"
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 18. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "a = b"
  shows "s p = e \<and> t (p a) = e \<and> u (p a c) = e"
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 19. DPE should eliminate p *)
lemma
  fixes a b p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "\<And>z. z e \<or> p (\<lambda>x. f x x) b"
    "b = c"
  shows "s p = e \<and> t (p a) = e \<and> u (p a c) = e"
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 20. DPE should eliminate p *)
lemma
  fixes a b e p q r
  assumes
    "\<And>x y. \<not> p x y \<or> q (\<lambda>y. y x) = e \<or> r y = e \<or> y a"
    "\<And>x y. p x y \<or> q (\<lambda>y. y x) \<noteq> e"
    "\<And>x y. p x y \<or> r y \<noteq> e"
    "\<And>x y. p x y \<or> \<not> y a"
    "\<And>z. z e \<or> p (\<lambda>x. f x x) b \<or> \<not> p c d"
    "b = c"
  shows "s p = e \<and> t (p a') = e \<and> u (p a' c') = e"
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* Finally, one polymorphic example *)

sledgehammer_params [type_enc = poly_native_higher_fool]

(* 21. DPE should eliminate p *)
lemma assms_21:
  "\<And>x y. \<not> p (x :: 'a) (y :: 'b) \<or> q (\<lambda>y. y x :: 'b) = e \<or> r y = e"
  "\<And>x y. p (x :: 'a) (y :: 'b) \<or> q (\<lambda>y. y x :: 'b) \<noteq> e"
  "\<And>x y. p x y \<or> r y \<noteq> e"
  sorry

lemma "s p = e \<and> t (p a) = e \<and> u (p a c) = e"
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_21) *)
  oops

end
