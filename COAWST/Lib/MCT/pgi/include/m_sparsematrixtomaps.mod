V26 m_sparsematrixtomaps
24 m_SparseMatrixToMaps.F90 S582 0
03/26/2019  11:41:43
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_string private
use m_sparsematrix private
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_string private
use m_sparsematrix private
enduse
D 303 24 1288 1056 1287 7
D 448 18 502
D 450 24 1623 280 1620 7
D 562 24 1623 280 1620 7
D 568 21 6 1 639 642 1 1 0 0 1
 3 640 3 3 640 641
D 571 21 6 1 644 650 0 1 0 0 1
 645 648 649 645 648 646
D 574 21 6 1 0 15 0 0 0 0 0
 0 15 0 3 15 0
D 577 21 6 1 652 658 0 1 0 0 1
 653 656 657 653 656 654
D 580 21 6 1 0 15 0 0 0 0 0
 0 15 0 3 15 0
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 m_sparsematrixtomaps
S 584 23 0 0 0 8 1287 582 4694 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrix
S 586 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 587 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 588 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 589 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 590 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 591 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 601 19 8 m_string tochar
R 602 19 9 m_string string_init
R 603 19 10 m_string init
R 604 19 11 m_string string_clean
R 605 19 12 m_string clean
R 606 19 13 m_string string_len
R 607 19 14 m_string string_bcast
R 608 19 15 m_string bcast
R 609 19 16 m_string string_mci
R 610 19 17 m_string string_mco
R 611 19 18 m_string ptr_chars
R 612 19 19 m_string char
R 620 19 27 m_string len
R 775 19 15 m_list init
R 776 19 16 m_list clean
R 777 19 17 m_list nullify
R 778 19 18 m_list get_indices
R 779 19 19 m_list test_indices
R 780 19 20 m_list nitem
R 781 19 21 m_list get
R 782 19 22 m_list identical
R 784 19 24 m_list copy
R 785 19 25 m_list exporttochar
R 786 19 26 m_list exporttostring
R 787 19 27 m_list charbuffersize
R 788 19 28 m_list append
R 789 19 29 m_list concatenate
R 790 19 30 m_list bcast
R 791 19 31 m_list send
R 792 19 32 m_list recv
R 793 19 33 m_list getsharedlistindices
R 799 19 39 m_list index
R 810 19 50 m_list allocated
R 989 19 22 m_attrvect init
R 990 19 23 m_attrvect clean
R 991 19 24 m_attrvect zero
R 992 19 25 m_attrvect lsize
R 993 19 26 m_attrvect niattr
R 994 19 27 m_attrvect nrattr
R 995 19 28 m_attrvect indexia
R 996 19 29 m_attrvect indexra
R 997 19 30 m_attrvect getilist
R 998 19 31 m_attrvect getrlist
R 999 19 32 m_attrvect exportilist
R 1000 19 33 m_attrvect exportrlist
R 1001 19 34 m_attrvect exportilisttochar
R 1002 19 35 m_attrvect exportrlisttochar
R 1003 19 36 m_attrvect appendiattr
R 1004 19 37 m_attrvect appendrattr
R 1005 19 38 m_attrvect exportiattr
R 1006 19 39 m_attrvect exportrattr
R 1007 19 40 m_attrvect importiattr
R 1008 19 41 m_attrvect importrattr
R 1009 19 42 m_attrvect copy
R 1010 19 43 m_attrvect rcopy
R 1011 19 44 m_attrvect icopy
R 1012 19 45 m_attrvect sort
R 1013 19 46 m_attrvect permute
R 1014 19 47 m_attrvect unpermute
R 1015 19 48 m_attrvect sortpermute
R 1016 19 49 m_attrvect sharedattrindexlist
R 1287 25 3 m_sparsematrix sparsematrix
R 1288 5 4 m_sparsematrix nrows sparsematrix
R 1289 5 5 m_sparsematrix ncols sparsematrix
R 1290 5 6 m_sparsematrix data sparsematrix
R 1291 5 7 m_sparsematrix vecinit sparsematrix
R 1293 5 9 m_sparsematrix row_s sparsematrix
R 1294 5 10 m_sparsematrix row_s$sd sparsematrix
R 1295 5 11 m_sparsematrix row_s$p sparsematrix
R 1296 5 12 m_sparsematrix row_s$o sparsematrix
R 1298 5 14 m_sparsematrix row_e sparsematrix
R 1300 5 16 m_sparsematrix row_e$sd sparsematrix
R 1301 5 17 m_sparsematrix row_e$p sparsematrix
R 1302 5 18 m_sparsematrix row_e$o sparsematrix
R 1306 5 22 m_sparsematrix tcol sparsematrix
R 1307 5 23 m_sparsematrix tcol$sd sparsematrix
R 1308 5 24 m_sparsematrix tcol$p sparsematrix
R 1309 5 25 m_sparsematrix tcol$o sparsematrix
R 1313 5 29 m_sparsematrix twgt sparsematrix
R 1314 5 30 m_sparsematrix twgt$sd sparsematrix
R 1315 5 31 m_sparsematrix twgt$p sparsematrix
R 1316 5 32 m_sparsematrix twgt$o sparsematrix
R 1318 5 34 m_sparsematrix row_max sparsematrix
R 1319 5 35 m_sparsematrix row_min sparsematrix
R 1320 5 36 m_sparsematrix tbl_end sparsematrix
R 1321 19 37 m_sparsematrix init
R 1322 19 38 m_sparsematrix vecinit
R 1323 19 39 m_sparsematrix clean
R 1324 19 40 m_sparsematrix lsize
R 1325 19 41 m_sparsematrix indexia
R 1326 19 42 m_sparsematrix indexra
R 1327 19 43 m_sparsematrix nrows
R 1328 19 44 m_sparsematrix ncols
R 1329 19 45 m_sparsematrix exportglobalrowindices
R 1330 19 46 m_sparsematrix exportglobalcolumnindices
R 1331 19 47 m_sparsematrix exportlocalrowindices
R 1332 19 48 m_sparsematrix exportlocalcolumnindices
R 1333 19 49 m_sparsematrix exportmatrixelements
R 1334 19 50 m_sparsematrix importglobalrowindices
R 1335 19 51 m_sparsematrix importglobalcolumnindices
R 1336 19 52 m_sparsematrix importlocalrowindices
R 1337 19 53 m_sparsematrix importlocalcolumnindices
R 1338 19 54 m_sparsematrix importmatrixelements
R 1339 19 55 m_sparsematrix copy
R 1340 19 56 m_sparsematrix globalnumelements
R 1341 19 57 m_sparsematrix computesparsity
R 1342 19 58 m_sparsematrix local_row_range
R 1343 19 59 m_sparsematrix global_row_range
R 1344 19 60 m_sparsematrix local_col_range
R 1345 19 61 m_sparsematrix global_col_range
R 1346 19 62 m_sparsematrix checkbounds
R 1347 19 63 m_sparsematrix row_sum
R 1348 19 64 m_sparsematrix row_sum_check
R 1349 19 65 m_sparsematrix sort
R 1350 19 66 m_sparsematrix permute
R 1351 19 67 m_sparsematrix sortpermute
S 1611 19 0 0 0 8 1 582 8995 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 245 1 0 0 0 0 0 582 0 0 0 0 sparsematrixtoxglobalsegmap
O 1611 1 1613
S 1612 19 0 0 0 8 1 582 9023 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 247 1 0 0 0 0 0 582 0 0 0 0 sparsematrixtoyglobalsegmap
O 1612 1 1614
S 1613 27 0 0 0 8 1909 582 9051 10010 400000 A 0 0 0 0 0 0 300 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrixtoxglobalsegmap_
Q 1613 1611 0
S 1614 27 0 0 0 8 1916 582 9080 10010 400000 A 0 0 0 0 0 0 301 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrixtoyglobalsegmap_
Q 1614 1612 0
S 1615 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1616 3 0 0 0 448 0 1 0 0 0 A 0 0 0 0 0 0 0 0 9109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 25 4d 43 54 3a 3a 6d 5f 53 70 61 72 73 65 4d 61 74 72 69 78 54 6f 4d 61 70 73
S 1617 16 0 0 0 448 1 582 4996 14 440000 A 0 0 0 0 0 0 0 0 1616 503 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 1620 25 1 m_globalsegmap globalsegmap
R 1621 19 2 m_globalsegmap init
R 1622 19 3 m_globalsegmap clean
R 1623 5 4 m_globalsegmap comp_id globalsegmap
R 1624 5 5 m_globalsegmap gsize globalsegmap
R 1625 19 6 m_globalsegmap globalstorage
R 1626 19 7 m_globalsegmap processstorage
R 1627 19 8 m_globalsegmap orderedpoints
R 1628 19 9 m_globalsegmap lsize
R 1629 5 10 m_globalsegmap ngseg globalsegmap
R 1630 19 11 m_globalsegmap nlseg
R 1631 19 12 m_globalsegmap max_nlseg
R 1632 19 13 m_globalsegmap active_pes
R 1633 19 14 m_globalsegmap pelocs
R 1634 19 15 m_globalsegmap haloed
R 1635 19 16 m_globalsegmap rank
R 1636 19 17 m_globalsegmap sort
R 1637 19 18 m_globalsegmap permute
R 1638 19 19 m_globalsegmap sortpermute
R 1639 19 20 m_globalsegmap increasing
R 1640 19 21 m_globalsegmap copy
R 1642 5 23 m_globalsegmap start globalsegmap
R 1643 5 24 m_globalsegmap start$sd globalsegmap
R 1644 5 25 m_globalsegmap start$p globalsegmap
R 1645 5 26 m_globalsegmap start$o globalsegmap
R 1648 5 29 m_globalsegmap length globalsegmap
R 1649 5 30 m_globalsegmap length$sd globalsegmap
R 1650 5 31 m_globalsegmap length$p globalsegmap
R 1651 5 32 m_globalsegmap length$o globalsegmap
R 1654 5 35 m_globalsegmap pe_loc globalsegmap
R 1655 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1656 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1657 5 38 m_globalsegmap pe_loc$o globalsegmap
R 1666 19 47 m_globalsegmap comp_id
R 1668 19 49 m_globalsegmap gsize
R 1674 19 55 m_globalsegmap ngseg
S 1909 23 5 0 0 0 1915 582 9051 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sparsematrixtoxglobalsegmap_
S 1910 1 3 3 0 303 1 1909 8556 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1911 1 3 2 0 562 1 1909 10022 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xgsmap
S 1912 1 3 1 0 6 1 1909 5102 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1913 1 3 1 0 6 1 1909 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1914 1 3 1 0 6 1 1909 9178 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comp_id
S 1915 14 5 0 0 0 1 1909 9051 10 400000 A 0 0 0 0 0 0 0 549 5 0 0 0 0 0 0 0 0 0 0 0 0 67 0 582 0 0 0 0 sparsematrixtoxglobalsegmap_
F 1915 5 1910 1911 1912 1913 1914
S 1916 23 5 0 0 0 1922 582 9080 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sparsematrixtoyglobalsegmap_
S 1917 1 3 3 0 303 1 1916 8556 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1918 1 3 2 0 450 1 1916 10029 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ygsmap
S 1919 1 3 1 0 6 1 1916 5102 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1920 1 3 1 0 6 1 1916 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1921 1 3 1 0 6 1 1916 9178 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comp_id
S 1922 14 5 0 0 0 1 1916 9080 10 400000 A 0 0 0 0 0 0 0 555 5 0 0 0 0 0 0 0 0 0 0 0 0 211 0 582 0 0 0 0 sparsematrixtoyglobalsegmap_
F 1922 5 1917 1918 1919 1920 1921
S 1923 23 5 0 0 0 1929 582 10036 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 computesegments_
S 1924 7 3 1 0 568 1 1923 5858 20000014 10003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 indices
S 1925 1 3 1 0 6 1 1923 10053 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 num_indices
S 1926 1 3 2 0 6 1 1923 10065 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nsegs
S 1927 7 3 0 0 571 1 1923 10071 10800014 3014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1934 0 0 0 0 0 0 0 0 starts
S 1928 7 3 0 0 577 1 1923 10078 10800014 3014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1938 0 0 0 0 0 0 0 0 lengths
S 1929 14 5 0 0 0 1 1923 10036 20000010 400000 A 0 0 0 0 0 0 0 561 5 0 0 0 0 0 0 0 0 0 0 0 0 356 0 582 0 0 0 0 computesegments_
F 1929 5 1924 1925 1926 1927 1928
S 1930 6 1 0 0 6 1 1923 8935 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_0_3
S 1931 6 1 0 0 6 1 1923 8943 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_2_3
S 1932 6 1 0 0 6 1 1923 8951 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_3_2
S 1933 6 1 0 0 6 1 1923 10086 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1110
S 1934 8 1 0 0 574 1 1923 10095 40822014 1020 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 starts$sd
S 1938 8 1 0 0 580 1 1923 10137 40822014 1020 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lengths$sd
A 15 2 0 0 0 6 586 0 0 0 15 0 0 0 0 0 0 0 0 0
A 17 2 0 0 0 6 591 0 0 0 17 0 0 0 0 0 0 0 0 0
A 19 2 0 0 0 6 587 0 0 0 19 0 0 0 0 0 0 0 0 0
A 21 2 0 0 0 6 588 0 0 0 21 0 0 0 0 0 0 0 0 0
A 25 2 0 0 0 6 589 0 0 0 25 0 0 0 0 0 0 0 0 0
A 27 2 0 0 0 6 590 0 0 0 27 0 0 0 0 0 0 0 0 0
A 502 2 0 0 223 6 1615 0 0 0 502 0 0 0 0 0 0 0 0 0
A 503 2 0 0 0 448 1616 0 0 0 503 0 0 0 0 0 0 0 0 0
A 639 1 0 0 383 6 1932 0 0 0 0 0 0 0 0 0 0 0 0 0
A 640 1 0 0 0 6 1930 0 0 0 0 0 0 0 0 0 0 0 0 0
A 641 1 0 0 0 6 1933 0 0 0 0 0 0 0 0 0 0 0 0 0
A 642 1 0 0 0 6 1931 0 0 0 0 0 0 0 0 0 0 0 0 0
A 643 1 0 1 0 574 1934 0 0 0 0 0 0 0 0 0 0 0 0 0
A 644 10 0 0 33 6 643 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 645 10 0 0 644 6 643 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 646 10 0 0 645 6 643 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 21
A 647 4 0 0 555 6 646 0 3 0 0 0 0 2 0 0 0 0 0 0
A 648 4 0 0 0 6 645 0 647 0 0 0 0 1 0 0 0 0 0 0
A 649 10 0 0 646 6 643 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 25
A 650 10 0 0 649 6 643 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
A 651 1 0 1 0 580 1938 0 0 0 0 0 0 0 0 0 0 0 0 0
A 652 10 0 0 0 6 651 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 653 10 0 0 652 6 651 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 654 10 0 0 653 6 651 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 21
A 655 4 0 0 0 6 654 0 3 0 0 0 0 2 0 0 0 0 0 0
A 656 4 0 0 515 6 653 0 655 0 0 0 0 1 0 0 0 0 0 0
A 657 10 0 0 654 6 651 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 25
A 658 10 0 0 657 6 651 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
Z
Z
