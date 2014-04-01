.. _`ho_indexing` :

Higher-Order indexing
=====================

Ideas for indexing of ``HOTerm.t``. We draw some ideas from `fingerprint indexing`_
and mostly `path indexing`_.

.. _`fingerprint indexing` : http://www4.in.tum.de/~schulz/PAPERS/Schulz-DedTref-2010.pdf
.. _`path indexing` : http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.26.1122

The basic idea currently is to extend *path indexing* so that it can deal with
binders, extensible records and mainly *multisets*. To do so, we use an
extended representation of positions in terms, with a non-deterministic
variant:

- ``Left``
  go to the left part of an application: ``(l @ r).left = l``.
- ``Right``
  go to the right part of an application: ``(l @ r).right = r``.
- ``Record_field of string``
  access the given record field, as in ``{x:t | _}.x = t``.
- ``Record_rest``
  access the rest of the extensible record. Basically ``{_ | R}.rest = R``.
- ``multiset of int``
  access **some** element of a multiset that has ``n`` elements. Here
  the choice is non-deterministic, and insertion and retrieval operations
  will need to take it into account.
