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
  sledgehammer [zipperposition, overlord, dont_slice] (assms)
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

end
