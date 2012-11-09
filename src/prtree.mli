(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*s This module provides a generic ASCII pretty-printing function for trees,
    in a way similar to what the Unix command \texttt{pstree} does:
\begin{verbatim}
bash-+-emacs-+-emacsserver
     |       `-ispell
     |-pstree
     `-xdvi.bin
\end{verbatim} *)

(*s A tree structure is given as an abstract type [t] together with a
    decomposition function [decomp] returning the label of the node and
    the list of the children trees. Leaves are nodes with no child (i.e.
    an empty list). *)

module type Tree = sig
  type t
  val decomp : t -> string * t list
end

(*s The functor [Make] takes a tree structure [T] as argument and provides a
    single function [print: formatter -> T.t -> unit] to print a tree on a
    given formatter. *)

module Make : functor (T : Tree) -> sig
  val print : Format.formatter -> T.t -> unit
end

(* \newpage *)
