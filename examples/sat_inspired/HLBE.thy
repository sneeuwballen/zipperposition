(* Isabelle/HOL file used to generate hlbe*.p. *)

(* Options:
   --debug.zip.hlb-elim=1000 \
   --hlbe-elim=true \
   --hlbe-track-eq=false \
   --hlbe-max-depth=2 \
   --hlbe-max-self-implications=2 \
   --hlbe-clauses-to-track=all \
   --hlbe-elim-max-tracked=250 *)

theory HLBE
  imports Main
begin

(* 1. HLE should simplify "cyan \<or> blue \<or> yellow" to "blue \<or> yellow" *)
lemma
  assumes
    "cyan \<longrightarrow> blue"
    "cyan \<longrightarrow> green"
    "cyan \<or> blue \<or> yellow"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 2. FLE should simplify "blue \<or> yellow" to "yellow" *)
lemma
  assumes
    "blue \<longrightarrow> cyan"
    "blue \<longrightarrow> \<not> cyan"
    "blue \<or> yellow"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 3. CongHLE+ should simplify "s = t \<or> u s a = u t a \<or> C" to "u s a = u t a \<or> C" *)
lemma
  assumes
    "s = t \<or> u s a = u t a \<or> C"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 4. CongHLE- should simplify "s \<noteq> t \<or> u s a \<noteq> u t a \<or> C" to "s \<noteq> t \<or> C" *)
lemma
  assumes
    "s \<noteq> t \<or> u s a \<noteq> u t a \<or> C"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 5. UnitHLE should simplify "q \<or> C \<or> D" to "C \<or> D" *)
lemma
  assumes
    "\<forall>x. p x a"
    "p b a \<longrightarrow> \<not> q"
    "q \<or> C \<or> D"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 6. HTR should simplify "green \<or> blue \<or> yellow \<or> red" to "green \<or> blue" *)
lemma
  assumes
    "\<not> green \<longrightarrow> gray"
    "gray \<longrightarrow> blue"
    "green \<or> blue \<or> yellow \<or> red"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 7. FLR should simplify "blue \<or> C \<or> D" to "blue" *)
lemma
  assumes
    "cyan \<longrightarrow> blue"
    "\<not> cyan \<longrightarrow> green"
    "green \<longrightarrow> blue"
    "blue \<or> C \<or> D"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 8. UnitHTR should simplify "q \<or> C \<or> D" to "q" *)
lemma
  assumes
    "\<forall>x. p x a"
    "p b a \<longrightarrow> q"
    "q \<or> C \<or> D"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* Same as above, but more higher-order *)

(* 9. HLE *)
lemma
  assumes
    "cyan (\<lambda>x :: nat. x) \<longrightarrow> blue rev"
    "cyan (\<lambda>x :: nat. x) \<longrightarrow> green"
    "cyan (\<lambda>x :: nat. x) \<or> blue rev \<or> yellow"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 10. FLE *)
lemma
  assumes
    "blue rev \<longrightarrow> cyan (\<lambda>x :: nat. x)"
    "blue rev \<longrightarrow> \<not> cyan (\<lambda>x :: nat. x)"
    "blue rev \<or> yellow"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 11. CongHLE+: doesn't work because too higher-order *)
lemma
  assumes
    "s = t \<or> u (\<lambda>x :: nat. s) a = u (\<lambda>x :: nat. t) a \<or> C"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 12. CongHLE-: this one works but apparently due to another optimization *)
lemma
  assumes
    "s \<noteq> t \<or> u (\<lambda>x :: nat. s) a \<noteq> u (\<lambda>x :: nat. t) a \<or> C"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 13. UnitHLE *)
lemma
  assumes
    "\<forall>x. p (\<lambda>y. y x) a"
    "p (\<lambda>y. y b) a \<longrightarrow> \<not> q"
    "q \<or> C \<or> D"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 14. HTR *)
lemma
  assumes
    "\<not> green (\<lambda>x :: nat. x) \<longrightarrow> gray"
    "gray \<longrightarrow> blue"
    "green (\<lambda>x :: nat. x) \<or> blue \<or> yellow \<or> red"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 15. FLR *)
lemma
  assumes
    "cyan (\<lambda>x :: nat. x) \<longrightarrow> blue"
    "\<not> cyan (\<lambda>x :: nat. x) \<longrightarrow> green"
    "green \<longrightarrow> blue"
    "blue \<or> C \<or> D"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 16. UnitHTR *)
lemma
  assumes
    "\<forall>x. p (\<lambda>y. y x) a"
    "p (\<lambda>y. y b) a \<longrightarrow> q"
    "q \<or> C \<or> D"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* Same as 1–8 above, but more polymorphic *)

sledgehammer_params [type_enc = poly_native_higher_fool]

axiomatization
  C :: bool and
  D :: bool and
  a :: 'a and
  b :: 'a and
  blue :: "'a \<Rightarrow> bool" and
  cyan :: "'a \<Rightarrow> bool" and
  gray :: "'a \<Rightarrow> bool" and
  green :: "'a \<Rightarrow> bool" and
  red :: "'a \<Rightarrow> bool" and
  yellow :: "'a \<Rightarrow> bool" and
  p :: 'a and
  q :: 'a and
  s :: 'a and
  t :: 'a and
  u :: 'a and
  z :: nat

(* 17. HLE *)
lemma assms_17:
  "cyan z \<longrightarrow> blue z"
  "cyan z \<longrightarrow> green z"
  "cyan z \<or> blue z \<or> yellow z"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_17) *)
  oops

(* 18. FLE *)
lemma assms_18:
  "blue z \<longrightarrow> cyan z"
  "blue z \<longrightarrow> \<not> cyan z"
  "blue z \<or> yellow z"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_18) *)
  oops

(* 19. CongHLE+ *)
lemma assms_19:
  "(s :: nat) = t \<or> u (s :: nat) (a :: nat) = u (t :: nat) (a :: nat) \<or> C"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_19) *)
  oops

(* 20. CongHLE- *)
lemma assms_20:
  "(s :: nat) \<noteq> t \<or> u (s :: nat) (a :: nat) \<noteq> u (t :: nat) (a :: nat) \<or> C"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_20) *)
  oops

(* 21. UnitHLE *)
lemma assms_21:
  "\<forall>x. p x a"
  "p (b :: nat) a \<longrightarrow> \<not> q"
  "q \<or> C \<or> D"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_21) *)
  oops

(* 22. HTR *)
lemma assms_22:
  "\<not> green z \<longrightarrow> gray z"
  "gray z \<longrightarrow> blue z"
  "green z \<or> blue z \<or> yellow z \<or> red z"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_22) *)
  oops

(* 23. FLR *)
lemma assms_23:
  "cyan z \<longrightarrow> blue z"
  "\<not> cyan z \<longrightarrow> green z"
  "green z \<longrightarrow> blue z"
  "blue z \<or> C \<or> D"
  sorry

lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_23) *)
  oops

lemma assms_24:
  "\<forall>x. p x a"
  "p b a \<longrightarrow> q"
  "q \<or> C \<or> D"
  sorry

(* 24. UnitHTR *)
lemma False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms_24) *)
  oops

end
