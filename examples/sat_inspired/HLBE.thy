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

(* FLE should simplify "blue \<or> yellow" to "yellow". *)
lemma
  assumes
    "blue \<longrightarrow> cyan"
    "blue \<longrightarrow> \<not> cyan"
    "blue \<or> yellow"
  shows False
  oops

(* CongHLE+ should simplify "s = t \<or> u s a = u t a \<or> C" to "u s a = u t a \<or> C" *)
lemma
  assumes
    "s = t \<or> u s a = u t a \<or> C"
  shows False
  oops

(* CongHLE- should simplify "s \<noteq> t \<or> u s a \<noteq> u t a \<or> C" to "s \<noteq> t \<or> C" *)
lemma
  assumes
    "s \<noteq> t \<or> u s a \<noteq> u t a \<or> C"
  shows False
  oops

(* UnitHLE should simplify "q \<or> C \<or> D" to "C \<or> D" *)
lemma
  assumes
    "\<forall>x. p x a"
    "p b a \<longrightarrow> \<not> q"
    "q \<or> C \<or> D"
  shows False
  oops

(* HTR should simplify "green \<or> blue \<or> yellow \<or> red" to "green \<or> blue" *)
lemma
  assumes
    "\<not> green \<longrightarrow> gray"
    "gray \<longrightarrow> blue"
    "green \<or> blue \<or> yellow \<or> red"
  shows False
  oops

(* FLR should simplify "blue \<or> C \<or> D" to "blue" *)
lemma
  assumes
    "cyan \<longrightarrow> blue"
    "\<not> cyan \<longrightarrow> green"
    "green \<longrightarrow> blue"
    "blue \<or> C \<or> D"
  shows False
  oops

(* UnitHTR should simplify "q \<or> C \<or> D" to "q" *)
lemma
  assumes
    "\<forall>x. p x a"
    "p b a \<longrightarrow> q"
    "q \<or> C \<or> D"
  shows False
  sledgehammer [zipperposition, overlord, dont_slice] (assms)
  oops

end
