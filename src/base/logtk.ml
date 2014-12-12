
(*
Copyright (c) 2013-2014, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Contains every other module} *)

module Symbol            = LogtkSymbol
module ScopedTerm        = LogtkScopedTerm
module FOTerm            = LogtkFOTerm
module Type              = LogtkType
module Util              = LogtkUtil
module PrologTerm        = LogtkPrologTerm
module Interfaces        = LogtkInterfaces
module DBEnv             = LogtkDBEnv
module Position          = LogtkPosition
module HOTerm            = LogtkHOTerm
module Formula           = LogtkFormula
module Substs            = LogtkSubsts
module Unif              = LogtkUnif
module Signature         = LogtkSignature
module TypeInference     = LogtkTypeInference
module Options           = LogtkOptions
module Comparison        = LogtkComparison
module Precedence        = LogtkPrecedence
module Precedence_intf   = LogtkPrecedence_intf
module Ordering          = LogtkOrdering
module Ordering_intf     = LogtkOrdering_intf
module Skolem            = LogtkSkolem
module Cnf               = LogtkCnf
module HORewriting       = LogtkHORewriting
module Index             = LogtkIndex
module Dtree             = LogtkDtree
module Fingerprint       = LogtkFingerprint
module NPDtree           = LogtkNPDtree
module Congruence        = LogtkCongruence
module FastFingerprint   = LogtkFastFingerprint
module FeatureVector     = LogtkFeatureVector
module Rewriting         = LogtkRewriting
module FormulaShape      = LogtkFormulaShape
module Transform         = LogtkTransform
module Lambda            = LogtkLambda
module Sourced           = LogtkSourced
module TypedPrologTerm   = LogtkTypedPrologTerm
module Hashcons          = LogtkHashcons
module Cache             = LogtkCache
module ParseLocation     = LogtkParseLocation
module Multiset          = LogtkMultiset
module PartialOrder      = LogtkPartialOrder
module PartialOrder_intf = LogtkPartialOrder_intf
module LazyList          = LogtkLazyList
module IArray            = LogtkIArray
module LazyGraph         = LogtkLazyGraph
