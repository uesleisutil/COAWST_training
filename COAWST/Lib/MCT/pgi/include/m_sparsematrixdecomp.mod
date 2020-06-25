V26 m_sparsematrixdecomp
24 m_SparseMatrixDecomp.F90 S582 0
03/26/2019  11:41:41
use m_attrvect private
use m_list private
use m_string private
use m_sparsematrix private
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_string private
use m_sparsematrix private
use m_globalsegmap private
enduse
D 56 18 12
D 305 24 1294 1056 1293 7
D 450 24 1622 280 1619 7
D 562 24 1622 280 1619 7
D 568 24 1294 1056 1293 7
D 574 21 6 1 639 642 1 1 0 0 1
 3 640 3 3 640 641
D 577 21 6 1 643 646 1 1 0 0 1
 3 644 3 3 644 645
D 580 21 6 1 648 654 0 1 0 0 1
 649 652 653 649 652 650
D 583 21 6 1 0 34 0 0 0 0 0
 0 34 0 3 34 0
D 586 21 6 1 656 662 0 1 0 0 1
 657 660 661 657 660 658
D 589 21 6 1 0 34 0 0 0 0 0
 0 34 0 3 34 0
D 592 21 6 1 664 670 0 1 0 0 1
 665 668 669 665 668 666
D 595 21 6 1 0 34 0 0 0 0 0
 0 34 0 3 34 0
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 m_sparsematrixdecomp
S 583 19 0 0 0 8 1 582 4679 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0 582 0 0 0 0 bycolumn
O 583 1 585
S 584 19 0 0 0 8 1 582 4688 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 1 0 0 0 0 0 582 0 0 0 0 byrow
O 584 1 586
S 585 27 0 0 0 8 1908 582 4694 10010 400000 A 0 0 0 0 0 0 299 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 bycolumngsmap_
Q 585 583 0
S 586 27 0 0 0 8 1915 582 4709 10010 400000 A 0 0 0 0 0 0 300 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 byrowgsmap_
Q 586 584 0
S 587 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 588 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 4721 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 25 4d 43 54 3a 3a 6d 5f 53 70 61 72 73 65 4d 61 74 72 69 78 44 65 63 6f 6d 70
S 589 16 0 0 0 56 1 582 4747 14 440000 A 0 0 0 0 0 0 0 0 588 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 592 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 606 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 607 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 608 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 609 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 610 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 620 19 8 m_string tochar
R 621 19 9 m_string string_init
R 622 19 10 m_string init
R 623 19 11 m_string string_clean
R 624 19 12 m_string clean
R 625 19 13 m_string string_len
R 626 19 14 m_string string_bcast
R 627 19 15 m_string bcast
R 628 19 16 m_string string_mci
R 629 19 17 m_string string_mco
R 630 19 18 m_string ptr_chars
R 631 19 19 m_string char
R 639 19 27 m_string len
R 794 19 15 m_list init
R 795 19 16 m_list clean
R 796 19 17 m_list nullify
R 797 19 18 m_list get_indices
R 798 19 19 m_list test_indices
R 799 19 20 m_list nitem
R 800 19 21 m_list get
R 801 19 22 m_list identical
R 803 19 24 m_list copy
R 804 19 25 m_list exporttochar
R 805 19 26 m_list exporttostring
R 806 19 27 m_list charbuffersize
R 807 19 28 m_list append
R 808 19 29 m_list concatenate
R 809 19 30 m_list bcast
R 810 19 31 m_list send
R 811 19 32 m_list recv
R 812 19 33 m_list getsharedlistindices
R 818 19 39 m_list index
R 829 19 50 m_list allocated
R 994 19 22 m_attrvect init
R 995 19 23 m_attrvect clean
R 996 19 24 m_attrvect zero
R 997 19 25 m_attrvect lsize
R 998 19 26 m_attrvect niattr
R 999 19 27 m_attrvect nrattr
R 1000 19 28 m_attrvect indexia
R 1001 19 29 m_attrvect indexra
R 1002 19 30 m_attrvect getilist
R 1003 19 31 m_attrvect getrlist
R 1004 19 32 m_attrvect exportilist
R 1005 19 33 m_attrvect exportrlist
R 1006 19 34 m_attrvect exportilisttochar
R 1007 19 35 m_attrvect exportrlisttochar
R 1008 19 36 m_attrvect appendiattr
R 1009 19 37 m_attrvect appendrattr
R 1010 19 38 m_attrvect exportiattr
R 1011 19 39 m_attrvect exportrattr
R 1012 19 40 m_attrvect importiattr
R 1013 19 41 m_attrvect importrattr
R 1014 19 42 m_attrvect copy
R 1015 19 43 m_attrvect rcopy
R 1016 19 44 m_attrvect icopy
R 1017 19 45 m_attrvect sort
R 1018 19 46 m_attrvect permute
R 1019 19 47 m_attrvect unpermute
R 1020 19 48 m_attrvect sortpermute
R 1021 19 49 m_attrvect sharedattrindexlist
R 1293 25 3 m_sparsematrix sparsematrix
R 1294 5 4 m_sparsematrix nrows sparsematrix
R 1295 5 5 m_sparsematrix ncols sparsematrix
R 1296 5 6 m_sparsematrix data sparsematrix
R 1297 5 7 m_sparsematrix vecinit sparsematrix
R 1299 5 9 m_sparsematrix row_s sparsematrix
R 1300 5 10 m_sparsematrix row_s$sd sparsematrix
R 1301 5 11 m_sparsematrix row_s$p sparsematrix
R 1302 5 12 m_sparsematrix row_s$o sparsematrix
R 1304 5 14 m_sparsematrix row_e sparsematrix
R 1306 5 16 m_sparsematrix row_e$sd sparsematrix
R 1307 5 17 m_sparsematrix row_e$p sparsematrix
R 1308 5 18 m_sparsematrix row_e$o sparsematrix
R 1312 5 22 m_sparsematrix tcol sparsematrix
R 1313 5 23 m_sparsematrix tcol$sd sparsematrix
R 1314 5 24 m_sparsematrix tcol$p sparsematrix
R 1315 5 25 m_sparsematrix tcol$o sparsematrix
R 1319 5 29 m_sparsematrix twgt sparsematrix
R 1320 5 30 m_sparsematrix twgt$sd sparsematrix
R 1321 5 31 m_sparsematrix twgt$p sparsematrix
R 1322 5 32 m_sparsematrix twgt$o sparsematrix
R 1324 5 34 m_sparsematrix row_max sparsematrix
R 1325 5 35 m_sparsematrix row_min sparsematrix
R 1326 5 36 m_sparsematrix tbl_end sparsematrix
R 1327 19 37 m_sparsematrix init
R 1328 19 38 m_sparsematrix vecinit
R 1329 19 39 m_sparsematrix clean
R 1330 19 40 m_sparsematrix lsize
R 1331 19 41 m_sparsematrix indexia
R 1332 19 42 m_sparsematrix indexra
R 1333 19 43 m_sparsematrix nrows
R 1334 19 44 m_sparsematrix ncols
R 1335 19 45 m_sparsematrix exportglobalrowindices
R 1336 19 46 m_sparsematrix exportglobalcolumnindices
R 1337 19 47 m_sparsematrix exportlocalrowindices
R 1338 19 48 m_sparsematrix exportlocalcolumnindices
R 1339 19 49 m_sparsematrix exportmatrixelements
R 1340 19 50 m_sparsematrix importglobalrowindices
R 1341 19 51 m_sparsematrix importglobalcolumnindices
R 1342 19 52 m_sparsematrix importlocalrowindices
R 1343 19 53 m_sparsematrix importlocalcolumnindices
R 1344 19 54 m_sparsematrix importmatrixelements
R 1345 19 55 m_sparsematrix copy
R 1346 19 56 m_sparsematrix globalnumelements
R 1347 19 57 m_sparsematrix computesparsity
R 1348 19 58 m_sparsematrix local_row_range
R 1349 19 59 m_sparsematrix global_row_range
R 1350 19 60 m_sparsematrix local_col_range
R 1351 19 61 m_sparsematrix global_col_range
R 1352 19 62 m_sparsematrix checkbounds
R 1353 19 63 m_sparsematrix row_sum
R 1354 19 64 m_sparsematrix row_sum_check
R 1355 19 65 m_sparsematrix sort
R 1356 19 66 m_sparsematrix permute
R 1357 19 67 m_sparsematrix sortpermute
R 1619 25 1 m_globalsegmap globalsegmap
R 1620 19 2 m_globalsegmap init
R 1621 19 3 m_globalsegmap clean
R 1622 5 4 m_globalsegmap comp_id globalsegmap
R 1623 5 5 m_globalsegmap gsize globalsegmap
R 1624 19 6 m_globalsegmap globalstorage
R 1625 19 7 m_globalsegmap processstorage
R 1626 19 8 m_globalsegmap orderedpoints
R 1627 19 9 m_globalsegmap lsize
R 1628 5 10 m_globalsegmap ngseg globalsegmap
R 1629 19 11 m_globalsegmap nlseg
R 1630 19 12 m_globalsegmap max_nlseg
R 1631 19 13 m_globalsegmap active_pes
R 1632 19 14 m_globalsegmap pelocs
R 1633 19 15 m_globalsegmap haloed
R 1634 19 16 m_globalsegmap rank
R 1635 19 17 m_globalsegmap sort
R 1636 19 18 m_globalsegmap permute
R 1637 19 19 m_globalsegmap sortpermute
R 1638 19 20 m_globalsegmap increasing
R 1639 19 21 m_globalsegmap copy
R 1641 5 23 m_globalsegmap start globalsegmap
R 1642 5 24 m_globalsegmap start$sd globalsegmap
R 1643 5 25 m_globalsegmap start$p globalsegmap
R 1644 5 26 m_globalsegmap start$o globalsegmap
R 1647 5 29 m_globalsegmap length globalsegmap
R 1648 5 30 m_globalsegmap length$sd globalsegmap
R 1649 5 31 m_globalsegmap length$p globalsegmap
R 1650 5 32 m_globalsegmap length$o globalsegmap
R 1653 5 35 m_globalsegmap pe_loc globalsegmap
R 1654 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1655 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1656 5 38 m_globalsegmap pe_loc$o globalsegmap
R 1665 19 47 m_globalsegmap comp_id
R 1667 19 49 m_globalsegmap gsize
R 1673 19 55 m_globalsegmap ngseg
S 1908 23 5 0 0 0 1914 582 4694 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bycolumngsmap_
S 1909 1 3 1 0 562 1 1908 9950 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xgsmap
S 1910 1 3 3 0 568 1 1908 8624 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1911 1 3 2 0 562 1 1908 9957 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smgsmap
S 1912 1 3 1 0 6 1 1908 5229 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1913 1 3 1 0 6 1 1908 5234 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1914 14 5 0 0 0 1 1908 4694 10 400000 A 0 0 0 0 0 0 0 549 5 0 0 0 0 0 0 0 0 0 0 0 0 63 0 582 0 0 0 0 bycolumngsmap_
F 1914 5 1909 1910 1911 1912 1913
S 1915 23 5 0 0 0 1921 582 4709 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 byrowgsmap_
S 1916 1 3 1 0 450 1 1915 9965 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ygsmap
S 1917 1 3 3 0 305 1 1915 8624 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1918 1 3 2 0 450 1 1915 9957 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smgsmap
S 1919 1 3 1 0 6 1 1915 5229 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1920 1 3 1 0 6 1 1915 5234 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1921 14 5 0 0 0 1 1915 4709 10 400000 A 0 0 0 0 0 0 0 555 5 0 0 0 0 0 0 0 0 0 0 0 0 341 0 582 0 0 0 0 byrowgsmap_
F 1921 5 1916 1917 1918 1919 1920
S 1922 23 5 0 0 0 1930 582 9972 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 computesegments_
S 1923 7 3 1 0 574 1 1922 9989 20000014 10003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 element_pe_locs
S 1924 7 3 1 0 577 1 1922 10005 20000014 10003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 elements
S 1925 1 3 1 0 6 1 1922 10014 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 num_elements
S 1926 1 3 2 0 6 1 1922 10027 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nsegs
S 1927 7 3 0 0 580 1 1922 10033 10800014 3014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1939 0 0 0 0 0 0 0 0 seg_starts
S 1928 7 3 0 0 586 1 1922 10044 10800014 3014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1943 0 0 0 0 0 0 0 0 seg_lengths
S 1929 7 3 0 0 592 1 1922 10056 10800014 3014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1947 0 0 0 0 0 0 0 0 seg_pe_locs
S 1930 14 5 0 0 0 1 1922 9972 20000010 400000 A 0 0 0 0 0 0 0 561 7 0 0 0 0 0 0 0 0 0 0 0 0 622 0 582 0 0 0 0 computesegments_
F 1930 7 1923 1924 1925 1926 1927 1928 1929
S 1931 6 1 0 0 6 1 1922 9003 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_0_3
S 1932 6 1 0 0 6 1 1922 9011 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_2_3
S 1933 6 1 0 0 6 1 1922 9019 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_3_2
S 1934 6 1 0 0 6 1 1922 10068 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1106
S 1935 6 1 0 0 6 1 1922 10077 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_4_1
S 1936 6 1 0 0 6 1 1922 5278 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_6
S 1937 6 1 0 0 6 1 1922 9664 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_7
S 1938 6 1 0 0 6 1 1922 10085 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_1113
S 1939 8 1 0 0 583 1 1922 10094 40822014 1020 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 seg_starts$sd
S 1943 8 1 0 0 589 1 1922 10152 40822014 1020 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 seg_lengths$sd
S 1947 8 1 0 0 595 1 1922 10214 40822014 1020 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 seg_pe_locs$sd
A 12 2 0 0 0 6 587 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 56 588 0 0 0 13 0 0 0 0 0 0 0 0 0
A 19 2 0 0 0 6 592 0 0 0 19 0 0 0 0 0 0 0 0 0
A 34 2 0 0 0 6 606 0 0 0 34 0 0 0 0 0 0 0 0 0
A 37 2 0 0 0 6 607 0 0 0 37 0 0 0 0 0 0 0 0 0
A 39 2 0 0 0 6 608 0 0 0 39 0 0 0 0 0 0 0 0 0
A 43 2 0 0 0 6 609 0 0 0 43 0 0 0 0 0 0 0 0 0
A 45 2 0 0 0 6 610 0 0 0 45 0 0 0 0 0 0 0 0 0
A 639 1 0 0 0 6 1933 0 0 0 0 0 0 0 0 0 0 0 0 0
A 640 1 0 0 0 6 1931 0 0 0 0 0 0 0 0 0 0 0 0 0
A 641 1 0 0 109 6 1934 0 0 0 0 0 0 0 0 0 0 0 0 0
A 642 1 0 0 428 6 1932 0 0 0 0 0 0 0 0 0 0 0 0 0
A 643 1 0 0 201 6 1937 0 0 0 0 0 0 0 0 0 0 0 0 0
A 644 1 0 0 55 6 1935 0 0 0 0 0 0 0 0 0 0 0 0 0
A 645 1 0 0 0 6 1938 0 0 0 0 0 0 0 0 0 0 0 0 0
A 646 1 0 0 0 6 1936 0 0 0 0 0 0 0 0 0 0 0 0 0
A 647 1 0 1 0 583 1939 0 0 0 0 0 0 0 0 0 0 0 0 0
A 648 10 0 0 0 6 647 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 649 10 0 0 648 6 647 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 37
A 650 10 0 0 649 6 647 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 39
A 651 4 0 0 114 6 650 0 3 0 0 0 0 2 0 0 0 0 0 0
A 652 4 0 0 0 6 649 0 651 0 0 0 0 1 0 0 0 0 0 0
A 653 10 0 0 650 6 647 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 43
A 654 10 0 0 653 6 647 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 45
A 655 1 0 1 0 589 1943 0 0 0 0 0 0 0 0 0 0 0 0 0
A 656 10 0 0 47 6 655 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 657 10 0 0 656 6 655 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 37
A 658 10 0 0 657 6 655 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 39
A 659 4 0 0 559 6 658 0 3 0 0 0 0 2 0 0 0 0 0 0
A 660 4 0 0 0 6 657 0 659 0 0 0 0 1 0 0 0 0 0 0
A 661 10 0 0 658 6 655 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 43
A 662 10 0 0 661 6 655 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 45
A 663 1 0 1 0 595 1947 0 0 0 0 0 0 0 0 0 0 0 0 0
A 664 10 0 0 53 6 663 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 665 10 0 0 664 6 663 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 37
A 666 10 0 0 665 6 663 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 39
A 667 4 0 0 567 6 666 0 3 0 0 0 0 2 0 0 0 0 0 0
A 668 4 0 0 548 6 665 0 667 0 0 0 0 1 0 0 0 0 0 0
A 669 10 0 0 666 6 663 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 43
A 670 10 0 0 669 6 663 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 45
Z
Z
