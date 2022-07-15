(* Isabelle/HOL file used to generate bce*.p. *)

theory BCE
  imports Main
begin


lemma
  assumes
    "\<not> p \<or> q"
    "p \<or> \<not> q"
  shows False
  sledgehammer [zipperposition, overlord, dont_slice] (assms)
  oops



(* 1. "\<not> p \<or> q" or "p \<or> \<not> q" is blocked on the "p"-literal *)
lemma
  assumes
    "\<not> p \<or> q"
    "p \<or> \<not> q"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 2. "\<not> p \<or> q" or "p \<or> \<not> q" is blocked on the "p"-literal *)
lemma
  assumes
    "\<not> p \<or> q"
    "p \<or> \<not> q"
    "\<not> q \<or> a = b"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 3. neither clause is blocked *)
lemma
  assumes
    "\<And>x y. p x y \<or> p y x"
    "\<And>x y. \<not> p x y \<or> \<not> p y x"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

(* 4. either clause is blocked (on a "p"-literal) *)
lemma
  assumes
    "p a \<or> p b \<or> \<not> q"
    "\<And>x. \<not> p x \<or> q"
  shows False
  (* sledgehammer [zipperposition, overlord, dont_slice] (assms) *)
  oops

end
