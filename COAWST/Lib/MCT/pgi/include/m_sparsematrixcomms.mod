V26 m_sparsematrixcomms
23 m_SparseMatrixComms.F90 S582 0
03/26/2019  11:41:42
use m_attrvect private
use m_list private
use m_string private
use m_globalmap private
use m_sparsematrix private
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_string private
use m_globalmap private
use m_sparsematrix private
use m_globalsegmap private
enduse
D 56 18 12
D 305 24 1298 1056 1297 7
D 450 24 1626 280 1623 7
D 562 24 1626 280 1623 7
D 568 24 1298 1056 1297 7
D 605 24 1932 192 1931 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 m_sparsematrixcomms
S 583 19 0 0 0 8 1 582 4678 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0 582 0 0 0 0 scatterbycolumn
O 583 1 587
S 584 19 0 0 0 8 1 582 4694 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 1 0 0 0 0 0 582 0 0 0 0 scatterbyrow
O 584 1 588
S 585 19 0 0 0 8 1 582 4707 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 2 0 0 0 0 0 582 0 0 0 0 gather
O 585 2 590 589
S 586 19 0 0 0 8 1 582 4714 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 1 0 0 0 0 0 582 0 0 0 0 bcast
O 586 1 591
S 587 27 0 0 0 8 1912 582 4720 10010 400000 A 0 0 0 0 0 0 305 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 scatterbycolumngsmap_
Q 587 583 0
S 588 27 0 0 0 8 1920 582 4742 10010 400000 A 0 0 0 0 0 0 306 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 scatterbyrowgsmap_
Q 588 584 0
S 589 27 0 0 0 8 2023 582 4761 10010 400000 A 0 0 0 0 0 0 325 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_gather_
Q 589 585 0
S 590 27 0 0 0 8 2031 582 4772 10010 400000 A 0 0 0 0 0 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_gather_
Q 590 585 0
S 591 27 0 0 0 8 2039 582 4784 10010 400000 A 0 0 0 0 0 0 327 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 bcast_
Q 591 586 0
S 592 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 593 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 4791 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 24 4d 43 54 3a 3a 6d 5f 53 70 61 72 73 65 4d 61 74 72 69 78 43 6f 6d 6d 73
S 594 16 0 0 0 56 1 582 4816 14 440000 A 0 0 0 0 0 0 0 0 593 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 625 19 8 m_string tochar
R 626 19 9 m_string string_init
R 627 19 10 m_string init
R 628 19 11 m_string string_clean
R 629 19 12 m_string clean
R 630 19 13 m_string string_len
R 631 19 14 m_string string_bcast
R 632 19 15 m_string bcast
R 633 19 16 m_string string_mci
R 634 19 17 m_string string_mco
R 635 19 18 m_string ptr_chars
R 636 19 19 m_string char
R 644 19 27 m_string len
R 798 19 15 m_list init
R 799 19 16 m_list clean
R 800 19 17 m_list nullify
R 801 19 18 m_list get_indices
R 802 19 19 m_list test_indices
R 803 19 20 m_list nitem
R 804 19 21 m_list get
R 805 19 22 m_list identical
R 807 19 24 m_list copy
R 808 19 25 m_list exporttochar
R 809 19 26 m_list exporttostring
R 810 19 27 m_list charbuffersize
R 811 19 28 m_list append
R 812 19 29 m_list concatenate
R 813 19 30 m_list bcast
R 814 19 31 m_list send
R 815 19 32 m_list recv
R 816 19 33 m_list getsharedlistindices
R 822 19 39 m_list index
R 833 19 50 m_list allocated
R 998 19 22 m_attrvect init
R 999 19 23 m_attrvect clean
R 1000 19 24 m_attrvect zero
R 1001 19 25 m_attrvect lsize
R 1002 19 26 m_attrvect niattr
R 1003 19 27 m_attrvect nrattr
R 1004 19 28 m_attrvect indexia
R 1005 19 29 m_attrvect indexra
R 1006 19 30 m_attrvect getilist
R 1007 19 31 m_attrvect getrlist
R 1008 19 32 m_attrvect exportilist
R 1009 19 33 m_attrvect exportrlist
R 1010 19 34 m_attrvect exportilisttochar
R 1011 19 35 m_attrvect exportrlisttochar
R 1012 19 36 m_attrvect appendiattr
R 1013 19 37 m_attrvect appendrattr
R 1014 19 38 m_attrvect exportiattr
R 1015 19 39 m_attrvect exportrattr
R 1016 19 40 m_attrvect importiattr
R 1017 19 41 m_attrvect importrattr
R 1018 19 42 m_attrvect copy
R 1019 19 43 m_attrvect rcopy
R 1020 19 44 m_attrvect icopy
R 1021 19 45 m_attrvect sort
R 1022 19 46 m_attrvect permute
R 1023 19 47 m_attrvect unpermute
R 1024 19 48 m_attrvect sortpermute
R 1025 19 49 m_attrvect sharedattrindexlist
R 1297 25 3 m_sparsematrix sparsematrix
R 1298 5 4 m_sparsematrix nrows sparsematrix
R 1299 5 5 m_sparsematrix ncols sparsematrix
R 1300 5 6 m_sparsematrix data sparsematrix
R 1301 5 7 m_sparsematrix vecinit sparsematrix
R 1303 5 9 m_sparsematrix row_s sparsematrix
R 1304 5 10 m_sparsematrix row_s$sd sparsematrix
R 1305 5 11 m_sparsematrix row_s$p sparsematrix
R 1306 5 12 m_sparsematrix row_s$o sparsematrix
R 1308 5 14 m_sparsematrix row_e sparsematrix
R 1310 5 16 m_sparsematrix row_e$sd sparsematrix
R 1311 5 17 m_sparsematrix row_e$p sparsematrix
R 1312 5 18 m_sparsematrix row_e$o sparsematrix
R 1316 5 22 m_sparsematrix tcol sparsematrix
R 1317 5 23 m_sparsematrix tcol$sd sparsematrix
R 1318 5 24 m_sparsematrix tcol$p sparsematrix
R 1319 5 25 m_sparsematrix tcol$o sparsematrix
R 1323 5 29 m_sparsematrix twgt sparsematrix
R 1324 5 30 m_sparsematrix twgt$sd sparsematrix
R 1325 5 31 m_sparsematrix twgt$p sparsematrix
R 1326 5 32 m_sparsematrix twgt$o sparsematrix
R 1328 5 34 m_sparsematrix row_max sparsematrix
R 1329 5 35 m_sparsematrix row_min sparsematrix
R 1330 5 36 m_sparsematrix tbl_end sparsematrix
R 1331 19 37 m_sparsematrix init
R 1332 19 38 m_sparsematrix vecinit
R 1333 19 39 m_sparsematrix clean
R 1334 19 40 m_sparsematrix lsize
R 1335 19 41 m_sparsematrix indexia
R 1336 19 42 m_sparsematrix indexra
R 1337 19 43 m_sparsematrix nrows
R 1338 19 44 m_sparsematrix ncols
R 1339 19 45 m_sparsematrix exportglobalrowindices
R 1340 19 46 m_sparsematrix exportglobalcolumnindices
R 1341 19 47 m_sparsematrix exportlocalrowindices
R 1342 19 48 m_sparsematrix exportlocalcolumnindices
R 1343 19 49 m_sparsematrix exportmatrixelements
R 1344 19 50 m_sparsematrix importglobalrowindices
R 1345 19 51 m_sparsematrix importglobalcolumnindices
R 1346 19 52 m_sparsematrix importlocalrowindices
R 1347 19 53 m_sparsematrix importlocalcolumnindices
R 1348 19 54 m_sparsematrix importmatrixelements
R 1349 19 55 m_sparsematrix copy
R 1350 19 56 m_sparsematrix globalnumelements
R 1351 19 57 m_sparsematrix computesparsity
R 1352 19 58 m_sparsematrix local_row_range
R 1353 19 59 m_sparsematrix global_row_range
R 1354 19 60 m_sparsematrix local_col_range
R 1355 19 61 m_sparsematrix global_col_range
R 1356 19 62 m_sparsematrix checkbounds
R 1357 19 63 m_sparsematrix row_sum
R 1358 19 64 m_sparsematrix row_sum_check
R 1359 19 65 m_sparsematrix sort
R 1360 19 66 m_sparsematrix permute
R 1361 19 67 m_sparsematrix sortpermute
R 1623 25 1 m_globalsegmap globalsegmap
R 1624 19 2 m_globalsegmap init
R 1625 19 3 m_globalsegmap clean
R 1626 5 4 m_globalsegmap comp_id globalsegmap
R 1627 5 5 m_globalsegmap gsize globalsegmap
R 1628 19 6 m_globalsegmap globalstorage
R 1629 19 7 m_globalsegmap processstorage
R 1630 19 8 m_globalsegmap orderedpoints
R 1631 19 9 m_globalsegmap lsize
R 1632 5 10 m_globalsegmap ngseg globalsegmap
R 1633 19 11 m_globalsegmap nlseg
R 1634 19 12 m_globalsegmap max_nlseg
R 1635 19 13 m_globalsegmap active_pes
R 1636 19 14 m_globalsegmap pelocs
R 1637 19 15 m_globalsegmap haloed
R 1638 19 16 m_globalsegmap rank
R 1639 19 17 m_globalsegmap sort
R 1640 19 18 m_globalsegmap permute
R 1641 19 19 m_globalsegmap sortpermute
R 1642 19 20 m_globalsegmap increasing
R 1643 19 21 m_globalsegmap copy
R 1645 5 23 m_globalsegmap start globalsegmap
R 1646 5 24 m_globalsegmap start$sd globalsegmap
R 1647 5 25 m_globalsegmap start$p globalsegmap
R 1648 5 26 m_globalsegmap start$o globalsegmap
R 1651 5 29 m_globalsegmap length globalsegmap
R 1652 5 30 m_globalsegmap length$sd globalsegmap
R 1653 5 31 m_globalsegmap length$p globalsegmap
R 1654 5 32 m_globalsegmap length$o globalsegmap
R 1657 5 35 m_globalsegmap pe_loc globalsegmap
R 1658 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1659 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1660 5 38 m_globalsegmap pe_loc$o globalsegmap
R 1669 19 47 m_globalsegmap comp_id
R 1671 19 49 m_globalsegmap gsize
R 1677 19 55 m_globalsegmap ngseg
S 1912 23 5 0 0 0 1919 582 4720 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 scatterbycolumngsmap_
S 1913 1 3 1 0 562 1 1912 10006 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 columngsmap
S 1914 1 3 3 0 568 1 1912 10018 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 1915 1 3 2 0 568 1 1912 10024 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 1916 1 3 1 0 6 1 1912 5285 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1917 1 3 1 0 6 1 1912 5290 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1918 1 3 2 0 6 1 1912 5295 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1919 14 5 0 0 0 1 1912 4720 10 400000 A 0 0 0 0 0 0 0 549 6 0 0 0 0 0 0 0 0 0 0 0 0 84 0 582 0 0 0 0 scatterbycolumngsmap_
F 1919 6 1913 1914 1915 1916 1917 1918
S 1920 23 5 0 0 0 1927 582 4742 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 scatterbyrowgsmap_
S 1921 1 3 1 0 450 1 1920 10030 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rowgsmap
S 1922 1 3 3 0 305 1 1920 10018 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 1923 1 3 2 0 305 1 1920 10024 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 1924 1 3 1 0 6 1 1920 5285 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1925 1 3 1 0 6 1 1920 5290 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1926 1 3 2 0 6 1 1920 5295 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1927 14 5 0 0 0 1 1920 4742 10 400000 A 0 0 0 0 0 0 0 556 6 0 0 0 0 0 0 0 0 0 0 0 0 249 0 582 0 0 0 0 scatterbyrowgsmap_
F 1927 6 1921 1922 1923 1924 1925 1926
R 1931 25 1 m_globalmap globalmap
R 1932 5 2 m_globalmap comp_id globalmap
R 1933 5 3 m_globalmap gsize globalmap
R 1934 5 4 m_globalmap lsize globalmap
R 1936 5 6 m_globalmap counts globalmap
R 1937 5 7 m_globalmap counts$sd globalmap
R 1938 5 8 m_globalmap counts$p globalmap
R 1939 5 9 m_globalmap counts$o globalmap
R 1942 5 12 m_globalmap displs globalmap
R 1943 5 13 m_globalmap displs$sd globalmap
R 1944 5 14 m_globalmap displs$p globalmap
R 1945 5 15 m_globalmap displs$o globalmap
R 1947 19 17 m_globalmap gsize
R 1948 19 18 m_globalmap lsize
R 1949 19 19 m_globalmap init
R 1950 19 20 m_globalmap init_remote
R 1951 19 21 m_globalmap clean
R 1952 19 22 m_globalmap rank
R 1953 19 23 m_globalmap bounds
R 1954 19 24 m_globalmap comp_id
S 2023 23 5 0 0 0 2030 582 4761 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_gather_
S 2024 1 3 1 0 305 1 2023 10024 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 2025 1 3 2 0 305 1 2023 10018 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 2026 1 3 1 0 605 1 2023 10262 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 2027 1 3 1 0 6 1 2023 5285 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2028 1 3 1 0 6 1 2023 5290 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2029 1 3 2 0 6 1 2023 5295 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 2030 14 5 0 0 0 1 2023 4761 10 400000 A 0 0 0 0 0 0 0 599 6 0 0 0 0 0 0 0 0 0 0 0 0 406 0 582 0 0 0 0 gm_gather_
F 2030 6 2024 2025 2026 2027 2028 2029
S 2031 23 5 0 0 0 2038 582 4772 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_gather_
S 2032 1 3 1 0 305 1 2031 10024 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lsmat
S 2033 1 3 2 0 305 1 2031 10018 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmat
S 2034 1 3 1 0 450 1 2031 9692 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 2035 1 3 1 0 6 1 2031 5285 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2036 1 3 1 0 6 1 2031 5290 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2037 1 3 2 0 6 1 2031 5295 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 2038 14 5 0 0 0 1 2031 4772 10 400000 A 0 0 0 0 0 0 0 606 6 0 0 0 0 0 0 0 0 0 0 0 0 500 0 582 0 0 0 0 gsm_gather_
F 2038 6 2032 2033 2034 2035 2036 2037
S 2039 23 5 0 0 0 2044 582 4784 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcast_
S 2040 1 3 3 0 305 1 2039 8680 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2041 1 3 1 0 6 1 2039 5285 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2042 1 3 1 0 6 1 2039 5290 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2043 1 3 2 0 6 1 2039 5295 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 2044 14 5 0 0 0 1 2039 4784 10 400000 A 0 0 0 0 0 0 0 613 4 0 0 0 0 0 0 0 0 0 0 0 0 595 0 582 0 0 0 0 bcast_
F 2044 4 2040 2041 2042 2043
A 12 2 0 0 0 6 592 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 56 593 0 0 0 13 0 0 0 0 0 0 0 0 0
Z
Z
