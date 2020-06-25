V26 m_matattrvectmul
20 m_MatAttrVectMul.F90 S582 0
03/26/2019  11:41:44
use m_rearranger private
use m_router private
use m_globalsegmap private
use m_list private
use m_string private
use m_sparsematrix private
use m_sparsematrixplus private
use m_attrvect private
use m_rearranger private
use m_router private
use m_globalsegmap private
use m_list private
use m_string private
use m_sparsematrix private
use m_sparsematrixplus private
use m_attrvect private
enduse
D 56 18 12
D 196 24 977 624 976 7
D 450 24 977 624 976 7
D 456 24 1293 1056 1292 7
D 782 24 2112 6144 2111 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 0 0 0 0 m_matattrvectmul
S 583 19 0 0 0 8 1 582 4675 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 2 0 0 0 0 0 582 0 0 0 0 smatavmult
O 583 2 585 584
S 584 27 0 0 0 8 1616 582 4686 10010 400000 A 0 0 0 0 0 0 247 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 smatavmult_datalocal_
Q 584 583 0
S 585 27 0 0 0 8 2172 582 4708 10010 400000 A 0 0 0 0 0 0 328 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 smatavmult_smplus_
Q 585 583 0
S 586 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 21 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 587 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 4727 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 21 4d 43 54 3a 3a 6d 5f 4d 61 74 41 74 74 72 56 65 63 74 4d 75 6c
S 588 16 0 0 0 56 1 582 4749 14 440000 A 0 0 0 0 0 0 0 0 587 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 619 19 8 m_string tochar
R 620 19 9 m_string string_init
R 621 19 10 m_string init
R 622 19 11 m_string string_clean
R 623 19 12 m_string clean
R 624 19 13 m_string string_len
R 625 19 14 m_string string_bcast
R 626 19 15 m_string bcast
R 627 19 16 m_string string_mci
R 628 19 17 m_string string_mco
R 629 19 18 m_string ptr_chars
R 630 19 19 m_string char
R 638 19 27 m_string len
R 793 19 15 m_list init
R 794 19 16 m_list clean
R 795 19 17 m_list nullify
R 796 19 18 m_list get_indices
R 797 19 19 m_list test_indices
R 798 19 20 m_list nitem
R 799 19 21 m_list get
R 800 19 22 m_list identical
R 802 19 24 m_list copy
R 803 19 25 m_list exporttochar
R 804 19 26 m_list exporttostring
R 805 19 27 m_list charbuffersize
R 806 19 28 m_list append
R 807 19 29 m_list concatenate
R 808 19 30 m_list bcast
R 809 19 31 m_list send
R 810 19 32 m_list recv
R 811 19 33 m_list getsharedlistindices
R 817 19 39 m_list index
R 828 19 50 m_list allocated
R 976 25 5 m_attrvect attrvect
R 977 5 6 m_attrvect ilist attrvect
R 978 5 7 m_attrvect rlist attrvect
R 981 5 10 m_attrvect iattr attrvect
R 982 5 11 m_attrvect iattr$sd attrvect
R 983 5 12 m_attrvect iattr$p attrvect
R 984 5 13 m_attrvect iattr$o attrvect
R 988 5 17 m_attrvect rattr attrvect
R 989 5 18 m_attrvect rattr$sd attrvect
R 990 5 19 m_attrvect rattr$p attrvect
R 991 5 20 m_attrvect rattr$o attrvect
R 993 19 22 m_attrvect init
R 994 19 23 m_attrvect clean
R 995 19 24 m_attrvect zero
R 996 19 25 m_attrvect lsize
R 997 19 26 m_attrvect niattr
R 998 19 27 m_attrvect nrattr
R 999 19 28 m_attrvect indexia
R 1000 19 29 m_attrvect indexra
R 1001 19 30 m_attrvect getilist
R 1002 19 31 m_attrvect getrlist
R 1003 19 32 m_attrvect exportilist
R 1004 19 33 m_attrvect exportrlist
R 1005 19 34 m_attrvect exportilisttochar
R 1006 19 35 m_attrvect exportrlisttochar
R 1007 19 36 m_attrvect appendiattr
R 1008 19 37 m_attrvect appendrattr
R 1009 19 38 m_attrvect exportiattr
R 1010 19 39 m_attrvect exportrattr
R 1011 19 40 m_attrvect importiattr
R 1012 19 41 m_attrvect importrattr
R 1013 19 42 m_attrvect copy
R 1014 19 43 m_attrvect rcopy
R 1015 19 44 m_attrvect icopy
R 1016 19 45 m_attrvect sort
R 1017 19 46 m_attrvect permute
R 1018 19 47 m_attrvect unpermute
R 1019 19 48 m_attrvect sortpermute
R 1020 19 49 m_attrvect sharedattrindexlist
R 1292 25 3 m_sparsematrix sparsematrix
R 1293 5 4 m_sparsematrix nrows sparsematrix
R 1294 5 5 m_sparsematrix ncols sparsematrix
R 1295 5 6 m_sparsematrix data sparsematrix
R 1296 5 7 m_sparsematrix vecinit sparsematrix
R 1298 5 9 m_sparsematrix row_s sparsematrix
R 1299 5 10 m_sparsematrix row_s$sd sparsematrix
R 1300 5 11 m_sparsematrix row_s$p sparsematrix
R 1301 5 12 m_sparsematrix row_s$o sparsematrix
R 1303 5 14 m_sparsematrix row_e sparsematrix
R 1305 5 16 m_sparsematrix row_e$sd sparsematrix
R 1306 5 17 m_sparsematrix row_e$p sparsematrix
R 1307 5 18 m_sparsematrix row_e$o sparsematrix
R 1311 5 22 m_sparsematrix tcol sparsematrix
R 1312 5 23 m_sparsematrix tcol$sd sparsematrix
R 1313 5 24 m_sparsematrix tcol$p sparsematrix
R 1314 5 25 m_sparsematrix tcol$o sparsematrix
R 1318 5 29 m_sparsematrix twgt sparsematrix
R 1319 5 30 m_sparsematrix twgt$sd sparsematrix
R 1320 5 31 m_sparsematrix twgt$p sparsematrix
R 1321 5 32 m_sparsematrix twgt$o sparsematrix
R 1323 5 34 m_sparsematrix row_max sparsematrix
R 1324 5 35 m_sparsematrix row_min sparsematrix
R 1325 5 36 m_sparsematrix tbl_end sparsematrix
R 1326 19 37 m_sparsematrix init
R 1327 19 38 m_sparsematrix vecinit
R 1328 19 39 m_sparsematrix clean
R 1329 19 40 m_sparsematrix lsize
R 1330 19 41 m_sparsematrix indexia
R 1331 19 42 m_sparsematrix indexra
R 1332 19 43 m_sparsematrix nrows
R 1333 19 44 m_sparsematrix ncols
R 1334 19 45 m_sparsematrix exportglobalrowindices
R 1335 19 46 m_sparsematrix exportglobalcolumnindices
R 1336 19 47 m_sparsematrix exportlocalrowindices
R 1337 19 48 m_sparsematrix exportlocalcolumnindices
R 1338 19 49 m_sparsematrix exportmatrixelements
R 1339 19 50 m_sparsematrix importglobalrowindices
R 1340 19 51 m_sparsematrix importglobalcolumnindices
R 1341 19 52 m_sparsematrix importlocalrowindices
R 1342 19 53 m_sparsematrix importlocalcolumnindices
R 1343 19 54 m_sparsematrix importmatrixelements
R 1344 19 55 m_sparsematrix copy
R 1345 19 56 m_sparsematrix globalnumelements
R 1346 19 57 m_sparsematrix computesparsity
R 1347 19 58 m_sparsematrix local_row_range
R 1348 19 59 m_sparsematrix global_row_range
R 1349 19 60 m_sparsematrix local_col_range
R 1350 19 61 m_sparsematrix global_col_range
R 1351 19 62 m_sparsematrix checkbounds
R 1352 19 63 m_sparsematrix row_sum
R 1353 19 64 m_sparsematrix row_sum_check
R 1354 19 65 m_sparsematrix sort
R 1355 19 66 m_sparsematrix permute
R 1356 19 67 m_sparsematrix sortpermute
S 1616 23 5 0 0 0 1623 582 4686 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatavmult_datalocal_
S 1617 1 3 1 0 450 1 1616 9065 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xav
S 1618 1 3 3 0 456 1 1616 8626 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 1619 1 3 3 0 450 1 1616 9069 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 yav
S 1620 1 3 1 0 16 1 1616 7102 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vector
S 1621 1 3 1 0 28 1 1616 6186 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rlist
S 1622 1 3 1 0 28 1 1616 7109 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 trlist
S 1623 14 5 0 0 0 1 1616 4686 10 400000 A 0 0 0 0 0 0 0 440 6 0 0 0 0 0 0 0 0 0 0 0 0 96 0 582 0 0 0 0 smatavmult_datalocal_
F 1623 6 1617 1618 1619 1620 1621 1622
R 1627 19 2 m_globalsegmap init
R 1628 19 3 m_globalsegmap clean
R 1631 19 6 m_globalsegmap globalstorage
R 1632 19 7 m_globalsegmap processstorage
R 1633 19 8 m_globalsegmap orderedpoints
R 1634 19 9 m_globalsegmap lsize
R 1636 19 11 m_globalsegmap nlseg
R 1637 19 12 m_globalsegmap max_nlseg
R 1638 19 13 m_globalsegmap active_pes
R 1639 19 14 m_globalsegmap pelocs
R 1640 19 15 m_globalsegmap haloed
R 1641 19 16 m_globalsegmap rank
R 1642 19 17 m_globalsegmap sort
R 1643 19 18 m_globalsegmap permute
R 1644 19 19 m_globalsegmap sortpermute
R 1645 19 20 m_globalsegmap increasing
R 1646 19 21 m_globalsegmap copy
R 1672 19 47 m_globalsegmap comp_id
R 1674 19 49 m_globalsegmap gsize
R 1680 19 55 m_globalsegmap ngseg
R 2016 19 100 m_router init
R 2017 19 101 m_router clean
R 2018 19 102 m_router print
R 2065 19 14 m_rearranger init
R 2066 19 15 m_rearranger rearrange
R 2067 19 16 m_rearranger clean
R 2068 19 17 m_rearranger print
R 2111 25 4 m_sparsematrixplus sparsematrixplus
R 2112 5 5 m_sparsematrixplus strategy sparsematrixplus
R 2113 5 6 m_sparsematrixplus xprimelength sparsematrixplus
R 2114 5 7 m_sparsematrixplus xtoxprime sparsematrixplus
R 2115 5 8 m_sparsematrixplus yprimelength sparsematrixplus
R 2116 5 9 m_sparsematrixplus yprimetoy sparsematrixplus
R 2117 5 10 m_sparsematrixplus matrix sparsematrixplus
R 2118 5 11 m_sparsematrixplus tag sparsematrixplus
R 2119 19 12 m_sparsematrixplus init
R 2120 19 13 m_sparsematrixplus vecinit
R 2121 19 14 m_sparsematrixplus clean
R 2122 19 15 m_sparsematrixplus initialized
R 2123 19 16 m_sparsematrixplus exportstrategytochar
S 2172 23 5 0 0 0 2179 582 4708 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatavmult_smplus_
S 2173 1 3 1 0 196 1 2172 9065 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 xav
S 2174 1 3 3 0 782 1 2172 11495 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smatplus
S 2175 1 3 3 0 196 1 2172 9069 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 yav
S 2176 1 3 1 0 16 1 2172 7102 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vector
S 2177 1 3 1 0 28 1 2172 6186 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rlist
S 2178 1 3 1 0 28 1 2172 7109 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 trlist
S 2179 14 5 0 0 0 1 2172 4708 10 400000 A 0 0 0 0 0 0 0 621 6 0 0 0 0 0 0 0 0 0 0 0 0 427 0 582 0 0 0 0 smatavmult_smplus_
F 2179 6 2173 2174 2175 2176 2177 2178
A 12 2 0 0 0 6 586 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 56 587 0 0 0 13 0 0 0 0 0 0 0 0 0
Z
Z
