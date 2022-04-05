(* Isabelle/HOL file used to generate hlbe*.p. *)

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

end
