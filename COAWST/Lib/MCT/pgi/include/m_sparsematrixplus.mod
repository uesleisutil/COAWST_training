V26 m_sparsematrixplus
22 m_SparseMatrixPlus.F90 S582 0
03/26/2019  11:41:43
use m_router private
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_rearranger private
use m_sparsematrix private
use m_string private
use m_router private
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_rearranger private
use m_sparsematrix private
use m_string private
enduse
D 56 24 599 88 597 7
D 65 21 6 1 0 15 0 0 0 0 0
 0 15 0 3 15 0
D 303 24 1291 1056 1290 7
D 448 24 1619 280 1616 7
D 699 24 2043 2488 2042 7
D 722 24 2091 6144 2090 7
D 728 18 789
D 730 18 119
D 732 21 6 1 0 3 0 0 0 0 0
 0 3 0 3 3 0
D 735 18 802
S 582 24 0 0 0 6 1 0 4658 10005 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 104 0 0 0 0 0 0 m_sparsematrixplus
S 584 23 0 0 0 8 597 582 4686 4 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 string
S 586 23 0 0 0 8 1290 582 4708 4 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 sparsematrix
S 588 23 0 0 0 8 2042 582 4734 4 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 rearranger
S 589 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 597 25 1 m_string string
R 599 5 3 m_string c string
R 600 5 4 m_string c$sd string
R 601 5 5 m_string c$p string
R 602 5 6 m_string c$o string
R 604 19 8 m_string tochar
R 605 19 9 m_string string_init
R 606 19 10 m_string init
R 607 19 11 m_string string_clean
R 608 19 12 m_string clean
R 609 19 13 m_string string_len
R 610 19 14 m_string string_bcast
R 611 19 15 m_string bcast
R 612 19 16 m_string string_mci
R 613 19 17 m_string string_mco
R 614 19 18 m_string ptr_chars
R 615 19 19 m_string char
R 623 19 27 m_string len
S 641 14 5 0 0 6 1 0 5060 40003814 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pghpf_size
S 762 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 778 19 15 m_list init
R 779 19 16 m_list clean
R 780 19 17 m_list nullify
R 781 19 18 m_list get_indices
R 782 19 19 m_list test_indices
R 783 19 20 m_list nitem
R 784 19 21 m_list get
R 785 19 22 m_list identical
R 787 19 24 m_list copy
R 788 19 25 m_list exporttochar
R 789 19 26 m_list exporttostring
R 790 19 27 m_list charbuffersize
R 791 19 28 m_list append
R 792 19 29 m_list concatenate
R 793 19 30 m_list bcast
R 794 19 31 m_list send
R 795 19 32 m_list recv
R 796 19 33 m_list getsharedlistindices
R 802 19 39 m_list index
R 813 19 50 m_list allocated
R 992 19 22 m_attrvect init
R 993 19 23 m_attrvect clean
R 994 19 24 m_attrvect zero
R 995 19 25 m_attrvect lsize
R 996 19 26 m_attrvect niattr
R 997 19 27 m_attrvect nrattr
R 998 19 28 m_attrvect indexia
R 999 19 29 m_attrvect indexra
R 1000 19 30 m_attrvect getilist
R 1001 19 31 m_attrvect getrlist
R 1002 19 32 m_attrvect exportilist
R 1003 19 33 m_attrvect exportrlist
R 1004 19 34 m_attrvect exportilisttochar
R 1005 19 35 m_attrvect exportrlisttochar
R 1006 19 36 m_attrvect appendiattr
R 1007 19 37 m_attrvect appendrattr
R 1008 19 38 m_attrvect exportiattr
R 1009 19 39 m_attrvect exportrattr
R 1010 19 40 m_attrvect importiattr
R 1011 19 41 m_attrvect importrattr
R 1012 19 42 m_attrvect copy
R 1013 19 43 m_attrvect rcopy
R 1014 19 44 m_attrvect icopy
R 1015 19 45 m_attrvect sort
R 1016 19 46 m_attrvect permute
R 1017 19 47 m_attrvect unpermute
R 1018 19 48 m_attrvect sortpermute
R 1019 19 49 m_attrvect sharedattrindexlist
R 1290 25 3 m_sparsematrix sparsematrix
R 1291 5 4 m_sparsematrix nrows sparsematrix
R 1292 5 5 m_sparsematrix ncols sparsematrix
R 1293 5 6 m_sparsematrix data sparsematrix
R 1294 5 7 m_sparsematrix vecinit sparsematrix
R 1296 5 9 m_sparsematrix row_s sparsematrix
R 1297 5 10 m_sparsematrix row_s$sd sparsematrix
R 1298 5 11 m_sparsematrix row_s$p sparsematrix
R 1299 5 12 m_sparsematrix row_s$o sparsematrix
R 1301 5 14 m_sparsematrix row_e sparsematrix
R 1303 5 16 m_sparsematrix row_e$sd sparsematrix
R 1304 5 17 m_sparsematrix row_e$p sparsematrix
R 1305 5 18 m_sparsematrix row_e$o sparsematrix
R 1309 5 22 m_sparsematrix tcol sparsematrix
R 1310 5 23 m_sparsematrix tcol$sd sparsematrix
R 1311 5 24 m_sparsematrix tcol$p sparsematrix
R 1312 5 25 m_sparsematrix tcol$o sparsematrix
R 1316 5 29 m_sparsematrix twgt sparsematrix
R 1317 5 30 m_sparsematrix twgt$sd sparsematrix
R 1318 5 31 m_sparsematrix twgt$p sparsematrix
R 1319 5 32 m_sparsematrix twgt$o sparsematrix
R 1321 5 34 m_sparsematrix row_max sparsematrix
R 1322 5 35 m_sparsematrix row_min sparsematrix
R 1323 5 36 m_sparsematrix tbl_end sparsematrix
R 1324 19 37 m_sparsematrix init
R 1325 19 38 m_sparsematrix vecinit
R 1326 19 39 m_sparsematrix clean
R 1327 19 40 m_sparsematrix lsize
R 1328 19 41 m_sparsematrix indexia
R 1329 19 42 m_sparsematrix indexra
R 1330 19 43 m_sparsematrix nrows
R 1331 19 44 m_sparsematrix ncols
R 1332 19 45 m_sparsematrix exportglobalrowindices
R 1333 19 46 m_sparsematrix exportglobalcolumnindices
R 1334 19 47 m_sparsematrix exportlocalrowindices
R 1335 19 48 m_sparsematrix exportlocalcolumnindices
R 1336 19 49 m_sparsematrix exportmatrixelements
R 1337 19 50 m_sparsematrix importglobalrowindices
R 1338 19 51 m_sparsematrix importglobalcolumnindices
R 1339 19 52 m_sparsematrix importlocalrowindices
R 1340 19 53 m_sparsematrix importlocalcolumnindices
R 1341 19 54 m_sparsematrix importmatrixelements
R 1342 19 55 m_sparsematrix copy
R 1343 19 56 m_sparsematrix globalnumelements
R 1344 19 57 m_sparsematrix computesparsity
R 1345 19 58 m_sparsematrix local_row_range
R 1346 19 59 m_sparsematrix global_row_range
R 1347 19 60 m_sparsematrix local_col_range
R 1348 19 61 m_sparsematrix global_col_range
R 1349 19 62 m_sparsematrix checkbounds
R 1350 19 63 m_sparsematrix row_sum
R 1351 19 64 m_sparsematrix row_sum_check
R 1352 19 65 m_sparsematrix sort
R 1353 19 66 m_sparsematrix permute
R 1354 19 67 m_sparsematrix sortpermute
R 1616 25 1 m_globalsegmap globalsegmap
R 1617 19 2 m_globalsegmap init
R 1618 19 3 m_globalsegmap clean
R 1619 5 4 m_globalsegmap comp_id globalsegmap
R 1620 5 5 m_globalsegmap gsize globalsegmap
R 1621 19 6 m_globalsegmap globalstorage
R 1622 19 7 m_globalsegmap processstorage
R 1623 19 8 m_globalsegmap orderedpoints
R 1624 19 9 m_globalsegmap lsize
R 1625 5 10 m_globalsegmap ngseg globalsegmap
R 1626 19 11 m_globalsegmap nlseg
R 1627 19 12 m_globalsegmap max_nlseg
R 1628 19 13 m_globalsegmap active_pes
R 1629 19 14 m_globalsegmap pelocs
R 1630 19 15 m_globalsegmap haloed
R 1631 19 16 m_globalsegmap rank
R 1632 19 17 m_globalsegmap sort
R 1633 19 18 m_globalsegmap permute
R 1634 19 19 m_globalsegmap sortpermute
R 1635 19 20 m_globalsegmap increasing
R 1636 19 21 m_globalsegmap copy
R 1638 5 23 m_globalsegmap start globalsegmap
R 1639 5 24 m_globalsegmap start$sd globalsegmap
R 1640 5 25 m_globalsegmap start$p globalsegmap
R 1641 5 26 m_globalsegmap start$o globalsegmap
R 1644 5 29 m_globalsegmap length globalsegmap
R 1645 5 30 m_globalsegmap length$sd globalsegmap
R 1646 5 31 m_globalsegmap length$p globalsegmap
R 1647 5 32 m_globalsegmap length$o globalsegmap
R 1650 5 35 m_globalsegmap pe_loc globalsegmap
R 1651 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1652 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1653 5 38 m_globalsegmap pe_loc$o globalsegmap
R 1662 19 47 m_globalsegmap comp_id
R 1664 19 49 m_globalsegmap gsize
R 1670 19 55 m_globalsegmap ngseg
R 2006 19 100 m_router init
R 2007 19 101 m_router clean
R 2008 19 102 m_router print
R 2042 25 2 m_rearranger rearranger
R 2043 5 3 m_rearranger sendrouter rearranger
R 2044 5 4 m_rearranger recvrouter rearranger
R 2047 5 7 m_rearranger localpack rearranger
R 2048 5 8 m_rearranger localpack$sd rearranger
R 2049 5 9 m_rearranger localpack$p rearranger
R 2050 5 10 m_rearranger localpack$o rearranger
R 2052 5 12 m_rearranger localsize rearranger
R 2054 19 14 m_rearranger init
R 2055 19 15 m_rearranger rearrange
R 2056 19 16 m_rearranger clean
R 2057 19 17 m_rearranger print
S 2090 25 0 0 0 722 1 582 11119 10000004 800014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2120 0 0 0 582 0 0 0 0 sparsematrixplus
S 2091 5 0 0 0 56 2092 582 11136 800004 0 A 0 0 0 0 0 0 0 0 722 0 0 0 0 0 0 0 0 0 0 0 1 2091 0 582 0 0 0 0 strategy
S 2092 5 0 0 0 6 2093 582 11145 800004 0 A 0 0 0 0 0 88 0 0 722 0 0 0 0 0 0 0 0 0 0 0 2091 2092 0 582 0 0 0 0 xprimelength
S 2093 5 0 0 0 699 2094 582 11158 800004 0 A 0 0 0 0 0 96 0 0 722 0 0 0 0 0 0 0 0 0 0 0 2092 2093 0 582 0 0 0 0 xtoxprime
S 2094 5 0 0 0 6 2095 582 11168 800004 0 A 0 0 0 0 0 2584 0 0 722 0 0 0 0 0 0 0 0 0 0 0 2093 2094 0 582 0 0 0 0 yprimelength
S 2095 5 0 0 0 699 2096 582 11181 800004 0 A 0 0 0 0 0 2592 0 0 722 0 0 0 0 0 0 0 0 0 0 0 2094 2095 0 582 0 0 0 0 yprimetoy
S 2096 5 0 0 0 303 2097 582 11191 800004 0 A 0 0 0 0 0 5080 0 0 722 0 0 0 0 0 0 0 0 0 0 0 2095 2096 0 582 0 0 0 0 matrix
S 2097 5 0 0 0 6 1 582 11106 800004 0 A 0 0 0 0 0 6136 0 0 722 0 0 0 0 0 0 0 0 0 0 0 2096 2097 0 582 0 0 0 0 tag
S 2098 19 0 0 0 6 1 582 4814 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 317 2 0 0 0 0 0 582 0 0 0 0 init
O 2098 2 2104 2103
S 2099 19 0 0 0 8 1 582 7299 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 319 1 0 0 0 0 0 582 0 0 0 0 vecinit
O 2099 1 2105
S 2100 19 0 0 0 8 1 582 4832 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 321 1 0 0 0 0 0 582 0 0 0 0 clean
O 2100 1 2106
S 2101 19 0 0 0 6 1 582 11198 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 323 1 0 0 0 0 0 582 0 0 0 0 initialized
O 2101 1 2107
S 2102 19 0 0 0 8 1 582 11210 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 325 1 0 0 0 0 0 582 0 0 0 0 exportstrategytochar
O 2102 1 2108
S 2103 27 0 0 0 6 2121 582 11231 10000 400000 A 0 0 0 0 0 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 initfromroot_
Q 2103 2098 0
S 2104 27 0 0 0 6 2132 582 11245 10000 400000 A 0 0 0 0 0 0 327 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 initdistributed_
Q 2104 2098 0
S 2105 27 0 0 0 8 2142 582 7879 10000 400000 A 0 0 0 0 0 0 328 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 vecinit_
Q 2105 2099 0
S 2106 27 0 0 0 8 2145 582 4940 10000 400000 A 0 0 0 0 0 0 329 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 clean_
Q 2106 2100 0
S 2107 27 0 0 0 6 2149 582 11262 10000 400000 A 0 0 0 0 0 0 330 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 initialized_
Q 2107 2101 0
S 2108 27 0 0 0 8 2153 582 11275 10000 400000 A 0 0 0 0 0 0 331 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 exportstrategytochar_
Q 2108 2102 0
S 2109 16 0 0 0 728 1 582 11297 4 440000 A 0 0 0 0 0 0 0 0 2115 790 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 xonly
S 2110 16 0 0 0 728 1 582 11303 4 440000 A 0 0 0 0 0 0 0 0 2116 792 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 yonly
S 2111 16 0 0 0 728 1 582 11309 4 440000 A 0 0 0 0 0 0 0 0 2117 794 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 xandy
S 2112 16 0 0 0 6 1 582 10975 4 400000 A 0 0 0 0 0 0 0 0 700 787 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 defaulttag
S 2113 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 700 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 2114 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 2115 3 0 0 0 728 0 1 0 0 0 A 0 0 0 0 0 0 0 0 11315 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 5 58 6f 6e 6c 79
S 2116 3 0 0 0 728 0 1 0 0 0 A 0 0 0 0 0 0 0 0 11321 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 5 59 6f 6e 6c 79
S 2117 3 0 0 0 728 0 1 0 0 0 A 0 0 0 0 0 0 0 0 11327 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 5 58 61 6e 64 59
S 2118 3 0 0 0 730 0 1 0 0 0 A 0 0 0 0 0 0 0 0 11333 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 23 4d 43 54 3a 3a 6d 5f 53 70 61 72 73 65 4d 61 74 72 69 78 50 6c 75 73
S 2119 16 0 0 0 730 1 582 5018 4 440000 A 0 0 0 0 0 0 0 0 2118 796 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 2120 8 5 0 0 732 1 582 11357 40022004 1220 A 0 0 0 0 0 0 0 722 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 m_sparsematrixplus$sparsematrixplus$td
S 2121 23 5 0 0 0 2131 582 11231 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initfromroot_
S 2122 1 3 2 0 722 1 2121 11396 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2123 1 3 3 0 303 1 2121 8578 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2124 1 3 1 0 448 1 2121 11405 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xgsmap
S 2125 1 3 1 0 448 1 2121 11412 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ygsmap
S 2126 1 3 1 0 28 1 2121 11136 4 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 strategy
S 2127 1 3 1 0 6 1 2121 5124 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2128 1 3 1 0 6 1 2121 5129 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2129 1 3 1 0 6 1 2121 11419 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 componentid
S 2130 1 3 1 0 6 1 2121 11106 80000004 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2131 14 5 0 0 0 1 2121 11231 0 400000 A 0 0 0 0 0 0 0 586 9 0 0 0 0 0 0 0 0 0 0 0 0 212 0 582 0 0 0 0 initfromroot_
F 2131 9 2122 2123 2124 2125 2126 2127 2128 2129 2130
S 2132 23 5 0 0 0 2141 582 11245 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initdistributed_
S 2133 1 3 2 0 722 1 2132 11396 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2134 1 3 3 0 303 1 2132 8578 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2135 1 3 1 0 448 1 2132 11405 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xgsmap
S 2136 1 3 1 0 448 1 2132 11412 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ygsmap
S 2137 1 3 1 0 6 1 2132 5124 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 2138 1 3 1 0 6 1 2132 5129 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2139 1 3 1 0 6 1 2132 11419 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 componentid
S 2140 1 3 1 0 6 1 2132 11106 80000004 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2141 14 5 0 0 0 1 2132 11245 0 400000 A 0 0 0 0 0 0 0 596 8 0 0 0 0 0 0 0 0 0 0 0 0 408 0 582 0 0 0 0 initdistributed_
F 2141 8 2133 2134 2135 2136 2137 2138 2139 2140
S 2142 23 5 0 0 0 2144 582 7879 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vecinit_
S 2143 1 3 3 0 722 1 2142 11431 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatp
S 2144 14 5 0 0 0 1 2142 7879 0 400000 A 0 0 0 0 0 0 0 605 1 0 0 0 0 0 0 0 0 0 0 0 0 553 0 582 0 0 0 0 vecinit_
F 2144 1 2143
S 2145 23 5 0 0 0 2148 582 4940 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 clean_
S 2146 1 3 3 0 722 1 2145 11431 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatp
S 2147 1 3 2 0 6 1 2145 5938 80000004 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 2148 14 5 0 0 0 1 2145 4940 0 400000 A 0 0 0 0 0 0 0 607 2 0 0 0 0 0 0 0 0 0 0 0 0 595 0 582 0 0 0 0 clean_
F 2148 2 2146 2147
S 2149 23 5 0 0 16 2151 582 11262 4 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initialized_
S 2150 1 3 1 0 722 1 2149 11396 4 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2151 14 5 0 0 16 1 2149 11262 4 400000 A 0 0 0 0 0 0 0 610 1 0 0 2152 0 0 0 0 0 0 0 0 0 751 0 582 0 0 0 0 initialized_
F 2151 1 2150
S 2152 1 3 0 0 16 1 2149 11262 4 1003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 initialized_
S 2153 23 5 0 0 8 2155 582 11275 0 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 exportstrategytochar_
S 2154 6 3 1 0 722 1 2153 11396 800004 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2155 14 5 0 0 735 1 2153 11275 4 480000 A 0 0 0 0 0 0 0 612 1 0 0 2156 0 0 0 0 0 0 0 0 0 822 0 582 0 0 0 0 exportstrategytochar_
F 2155 1 2154
S 2156 1 3 0 0 735 1 2153 11275 4 1083000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 exportstrategytochar_
A 15 2 0 0 0 6 589 0 0 0 15 0 0 0 0 0 0 0 0 0
A 16 1 0 1 0 65 600 0 0 0 0 0 0 0 0 0 0 0 0 0
A 29 1 0 0 0 6 641 0 0 0 0 0 0 0 0 0 0 0 0 0
A 119 2 0 0 0 6 762 0 0 0 119 0 0 0 0 0 0 0 0 0
A 787 2 0 0 549 6 2113 0 0 0 787 0 0 0 0 0 0 0 0 0
A 789 2 0 0 551 6 2114 0 0 0 789 0 0 0 0 0 0 0 0 0
A 790 2 0 0 73 728 2115 0 0 0 790 0 0 0 0 0 0 0 0 0
A 792 2 0 0 650 728 2116 0 0 0 792 0 0 0 0 0 0 0 0 0
A 794 2 0 0 735 728 2117 0 0 0 794 0 0 0 0 0 0 0 0 0
A 796 2 0 0 439 730 2118 0 0 0 796 0 0 0 0 0 0 0 0 0
A 798 1 0 0 293 722 2154 0 0 0 0 0 0 0 0 0 0 0 0 0
A 799 1 0 0 528 56 2091 0 0 0 0 0 0 0 0 0 0 0 0 0
A 800 9 0 0 698 56 798 799 0 0 0 0 0 0 0 0 0 0 0 0
A 801 9 0 1 0 65 800 16 0 0 0 0 0 0 0 0 0 0 0 0
A 802 13 0 0 0 6 29 0 0 0 0 0 0 0 2 16 0 0 0 0
W 2 5 801
Z
Z
