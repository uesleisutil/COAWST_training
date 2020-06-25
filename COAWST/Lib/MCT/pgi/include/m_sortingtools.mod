V26 m_sortingtools
18 m_SortingTools.F90 S582 0
03/26/2019  11:41:19
use m_permuter private
use m_rankmerge private
use m_indexbin_logical private
use m_indexbin_char private
use m_indexbin_integer private
use m_mergesorts private
use m_permuter private
use m_rankmerge private
use m_indexbin_logical private
use m_indexbin_char private
use m_indexbin_integer private
use m_mergesorts private
enduse
D 552 18 677
S 582 24 0 0 0 6 1 0 4658 15 0 A 0 0 0 0 0 0 0 0 0 0 0 0 96 0 0 0 0 0 0 0 0 32 0 0 0 0 0 0 m_sortingtools
R 591 19 1 m_mergesorts indexset
R 592 19 2 m_mergesorts indexsort
R 782 19 1 m_indexbin_integer indexbin
R 829 19 1 m_indexbin_char indexbin
R 875 19 1 m_indexbin_logical indexbin
R 888 19 1 m_rankmerge rankset
R 889 19 2 m_rankmerge rankmerge
R 890 19 3 m_rankmerge indexedrankmerge
R 1074 19 1 m_permuter permute
R 1075 19 2 m_permuter unpermute
S 1677 16 0 0 0 552 0 582 4909 800014 440000 A 0 0 0 0 0 0 0 0 1679 678 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 1678 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1679 3 0 0 0 552 0 1 0 0 0 A 0 0 0 0 0 0 0 0 7057 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 25 4d 43 54 28 4d 50 45 55 29 3a 3a 6d 5f 53 6f 72 74 69 6e 67 54 6f 6f 6c 73
S 1680 23 0 0 0 8 1075 582 5881 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 unpermute
S 1681 23 0 0 0 8 1074 582 5873 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 permute
S 1682 23 0 0 0 6 890 582 5443 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 indexedrankmerge
S 1683 23 0 0 0 8 889 582 5433 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 rankmerge
S 1684 23 0 0 0 8 888 582 5425 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 rankset
S 1685 23 0 0 0 6 875 582 5227 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 indexbin
S 1686 23 0 0 0 6 829 582 5227 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 indexbin
S 1687 23 0 0 0 6 782 582 5227 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 indexbin
S 1688 23 0 0 0 6 592 582 4796 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 indexsort
S 1689 23 0 0 0 6 591 582 4787 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 indexset
A 677 2 0 0 398 6 1678 0 0 0 677 0 0 0 0 0 0 0 0 0
A 678 2 0 0 637 552 1679 0 0 0 678 0 0 0 0 0 0 0 0 0
Z
Z
