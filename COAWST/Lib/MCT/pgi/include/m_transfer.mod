V26 m_transfer
14 m_Transfer.F90 S582 0
03/26/2019  11:41:46
use m_die private
use m_mpif90 private
use m_globalsegmap private
use m_list private
use m_string private
use m_router private
use m_attrvect private
use m_mctworld private
use m_die private
use m_mpif90 private
use m_globalsegmap private
use m_list private
use m_string private
use m_router private
use m_attrvect private
use m_mctworld private
enduse
D 222 18 162
D 243 24 1089 624 1088 7
D 472 24 1703 1184 1702 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 m_transfer
S 584 23 0 0 0 6 973 582 4680 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 mctworld
S 585 23 0 0 0 8 990 582 4689 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 thismctworld
S 587 23 0 0 0 8 1088 582 4713 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 attrvect
S 588 19 0 0 0 6 1 582 4722 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 207 1 0 0 0 0 0 582 0 0 0 0 niattr
O 588 1 1202
S 589 19 0 0 0 6 1 582 4729 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 205 1 0 0 0 0 0 582 0 0 0 0 nrattr
O 589 1 1206
S 590 19 0 0 0 8 1 582 4736 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 203 1 0 0 0 0 0 582 0 0 0 0 permute
O 590 1 1359
S 591 19 0 0 0 8 1 582 4744 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 201 1 0 0 0 0 0 582 0 0 0 0 unpermute
O 591 1 1369
S 592 19 0 0 0 8 1 582 4754 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 199 3 0 0 0 0 0 582 0 0 0 0 attrvect_init
O 592 3 1186 1180 1175
S 594 19 0 0 0 8 1 582 4773 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 195 1 0 0 0 0 0 582 0 0 0 0 attrvect_copy
O 594 1 1340
S 596 19 0 0 0 8 1 582 4792 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 193 1 0 0 0 0 0 582 0 0 0 0 attrvect_clean
O 596 1 1190
S 598 19 0 0 0 6 1 582 4813 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 191 1 0 0 0 0 0 582 0 0 0 0 lsize
O 598 1 1193
S 600 23 0 0 0 8 1702 582 4828 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 router
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
S 970 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 973 25 2 m_mctworld mctworld
R 990 6 19 m_mctworld thismctworld
R 991 19 20 m_mctworld initialized
R 992 19 21 m_mctworld init
R 993 19 22 m_mctworld clean
R 994 19 23 m_mctworld numcomponents
R 995 19 24 m_mctworld componentnumprocs
R 996 19 25 m_mctworld componenttoworldrank
R 997 19 26 m_mctworld componentrootrank
R 1088 25 5 m_attrvect attrvect
R 1089 5 6 m_attrvect ilist attrvect
R 1090 5 7 m_attrvect rlist attrvect
R 1093 5 10 m_attrvect iattr attrvect
R 1094 5 11 m_attrvect iattr$sd attrvect
R 1095 5 12 m_attrvect iattr$p attrvect
R 1096 5 13 m_attrvect iattr$o attrvect
R 1100 5 17 m_attrvect rattr attrvect
R 1101 5 18 m_attrvect rattr$sd attrvect
R 1102 5 19 m_attrvect rattr$p attrvect
R 1103 5 20 m_attrvect rattr$o attrvect
R 1105 19 22 m_attrvect init
R 1106 19 23 m_attrvect clean
R 1107 19 24 m_attrvect zero
R 1108 19 25 m_attrvect lsize
R 1109 19 26 m_attrvect niattr
R 1110 19 27 m_attrvect nrattr
R 1111 19 28 m_attrvect indexia
R 1112 19 29 m_attrvect indexra
R 1113 19 30 m_attrvect getilist
R 1114 19 31 m_attrvect getrlist
R 1115 19 32 m_attrvect exportilist
R 1116 19 33 m_attrvect exportrlist
R 1117 19 34 m_attrvect exportilisttochar
R 1118 19 35 m_attrvect exportrlisttochar
R 1119 19 36 m_attrvect appendiattr
R 1120 19 37 m_attrvect appendrattr
R 1121 19 38 m_attrvect exportiattr
R 1122 19 39 m_attrvect exportrattr
R 1123 19 40 m_attrvect importiattr
R 1124 19 41 m_attrvect importrattr
R 1125 19 42 m_attrvect copy
R 1126 19 43 m_attrvect rcopy
R 1127 19 44 m_attrvect icopy
R 1128 19 45 m_attrvect sort
R 1129 19 46 m_attrvect permute
R 1130 19 47 m_attrvect unpermute
R 1131 19 48 m_attrvect sortpermute
R 1132 19 49 m_attrvect sharedattrindexlist
R 1175 14 92 m_attrvect init_
R 1180 14 97 m_attrvect initv_
R 1186 14 103 m_attrvect initl_
R 1190 14 107 m_attrvect clean_
R 1193 14 110 m_attrvect lsize_
R 1202 14 119 m_attrvect niattr_
R 1206 14 123 m_attrvect nrattr_
R 1340 14 257 m_attrvect copy_
R 1359 14 276 m_attrvect permute_
R 1369 14 286 m_attrvect unpermute_
R 1398 19 2 m_globalsegmap init
R 1399 19 3 m_globalsegmap clean
R 1402 19 6 m_globalsegmap globalstorage
R 1403 19 7 m_globalsegmap processstorage
R 1404 19 8 m_globalsegmap orderedpoints
R 1405 19 9 m_globalsegmap lsize
R 1407 19 11 m_globalsegmap nlseg
R 1408 19 12 m_globalsegmap max_nlseg
R 1409 19 13 m_globalsegmap active_pes
R 1410 19 14 m_globalsegmap pelocs
R 1411 19 15 m_globalsegmap haloed
R 1412 19 16 m_globalsegmap rank
R 1413 19 17 m_globalsegmap sort
R 1414 19 18 m_globalsegmap permute
R 1415 19 19 m_globalsegmap sortpermute
R 1416 19 20 m_globalsegmap increasing
R 1417 19 21 m_globalsegmap copy
R 1443 19 47 m_globalsegmap comp_id
R 1445 19 49 m_globalsegmap gsize
R 1451 19 55 m_globalsegmap ngseg
R 1702 25 16 m_router router
R 1703 5 17 m_router comp1id router
R 1704 5 18 m_router comp2id router
R 1705 5 19 m_router nprocs router
R 1706 5 20 m_router maxsize router
R 1707 5 21 m_router lavsize router
R 1708 5 22 m_router numiatt router
R 1709 5 23 m_router numratt router
R 1711 5 25 m_router pe_list router
R 1712 5 26 m_router pe_list$sd router
R 1713 5 27 m_router pe_list$p router
R 1714 5 28 m_router pe_list$o router
R 1717 5 31 m_router num_segs router
R 1718 5 32 m_router num_segs$sd router
R 1719 5 33 m_router num_segs$p router
R 1720 5 34 m_router num_segs$o router
R 1723 5 37 m_router locsize router
R 1724 5 38 m_router locsize$sd router
R 1725 5 39 m_router locsize$p router
R 1726 5 40 m_router locsize$o router
R 1729 5 43 m_router permarr router
R 1730 5 44 m_router permarr$sd router
R 1731 5 45 m_router permarr$p router
R 1732 5 46 m_router permarr$o router
R 1736 5 50 m_router seg_starts router
R 1737 5 51 m_router seg_starts$sd router
R 1738 5 52 m_router seg_starts$p router
R 1739 5 53 m_router seg_starts$o router
R 1743 5 57 m_router seg_lengths router
R 1744 5 58 m_router seg_lengths$sd router
R 1745 5 59 m_router seg_lengths$p router
R 1746 5 60 m_router seg_lengths$o router
R 1749 5 63 m_router rp1 router
R 1750 5 64 m_router rp1$sd router
R 1751 5 65 m_router rp1$p router
R 1752 5 66 m_router rp1$o router
R 1755 5 69 m_router ip1 router
R 1756 5 70 m_router ip1$sd router
R 1757 5 71 m_router ip1$p router
R 1758 5 72 m_router ip1$o router
R 1761 5 75 m_router ireqs router
R 1762 5 76 m_router ireqs$sd router
R 1763 5 77 m_router ireqs$p router
R 1764 5 78 m_router ireqs$o router
R 1766 5 80 m_router rreqs router
R 1768 5 82 m_router rreqs$sd router
R 1769 5 83 m_router rreqs$p router
R 1770 5 84 m_router rreqs$o router
R 1774 5 88 m_router istatus router
R 1775 5 89 m_router istatus$sd router
R 1776 5 90 m_router istatus$p router
R 1777 5 91 m_router istatus$o router
R 1779 5 93 m_router rstatus router
R 1782 5 96 m_router rstatus$sd router
R 1783 5 97 m_router rstatus$p router
R 1784 5 98 m_router rstatus$o router
R 1786 19 100 m_router init
R 1787 19 101 m_router clean
R 1788 19 102 m_router print
S 1973 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 600 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 2251 19 17 m_mpif90 mp_type
R 2252 19 18 m_mpif90 mp_init
R 2253 19 19 m_mpif90 mp_initialized
R 2254 19 20 m_mpif90 mp_finalize
R 2255 19 21 m_mpif90 mp_abort
R 2256 19 22 m_mpif90 mp_wtime
R 2257 19 23 m_mpif90 mp_wtick
R 2258 19 24 m_mpif90 mp_comm_size
R 2259 19 25 m_mpif90 mp_comm_rank
R 2260 19 26 m_mpif90 mp_comm_dup
R 2261 19 27 m_mpif90 mp_comm_free
R 2262 19 28 m_mpif90 mp_cart_create
R 2263 19 29 m_mpif90 mp_dims_create
R 2264 19 30 m_mpif90 mp_cart_coords
R 2265 19 31 m_mpif90 mp_cart_rank
R 2266 19 32 m_mpif90 mp_error_string
R 2267 19 33 m_mpif90 mp_perr
R 2477 19 1 m_die mp_perr
R 2478 19 2 m_die die
R 2479 19 3 m_die diex
R 2480 19 4 m_die perr
R 2481 19 5 m_die warn
R 2482 19 6 m_die perr_die
R 2483 19 7 m_die mp_die
R 2484 19 8 m_die mp_perr_die
S 2550 19 0 0 0 6 1 582 14721 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 348 1 0 0 0 0 0 582 0 0 0 0 isend
O 2550 1 2556
S 2551 19 0 0 0 8 1 582 5625 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 350 1 0 0 0 0 0 582 0 0 0 0 send
O 2551 1 2557
S 2552 19 0 0 0 8 1 582 14727 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 352 1 0 0 0 0 0 582 0 0 0 0 waitsend
O 2552 1 2558
S 2553 19 0 0 0 6 1 582 14736 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 354 1 0 0 0 0 0 582 0 0 0 0 irecv
O 2553 1 2559
S 2554 19 0 0 0 8 1 582 5630 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 356 1 0 0 0 0 0 582 0 0 0 0 recv
O 2554 1 2560
S 2555 19 0 0 0 8 1 582 14742 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 358 1 0 0 0 0 0 582 0 0 0 0 waitrecv
O 2555 1 2561
S 2556 27 0 0 0 6 2565 582 14751 10010 400000 A 0 0 0 0 0 0 359 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 isend_
Q 2556 2550 0
S 2557 27 0 0 0 8 2573 582 5859 10010 400000 A 0 0 0 0 0 0 361 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 send_
Q 2557 2551 0
S 2558 27 0 0 0 8 2570 582 14758 10010 400000 A 0 0 0 0 0 0 360 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 waitsend_
Q 2558 2552 0
S 2559 27 0 0 0 6 2578 582 14768 10010 400000 A 0 0 0 0 0 0 362 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 irecv_
Q 2559 2553 0
S 2560 27 0 0 0 8 2589 582 5865 10010 400000 A 0 0 0 0 0 0 364 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 recv_
Q 2560 2554 0
S 2561 27 0 0 0 8 2584 582 14775 10010 400000 A 0 0 0 0 0 0 363 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 waitrecv_
Q 2561 2555 0
S 2562 16 0 0 0 6 1 582 14785 14 400000 A 0 0 0 0 0 0 0 0 600 1013 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 defaulttag
S 2563 3 0 0 0 222 0 1 0 0 0 A 0 0 0 0 0 0 0 0 14796 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 15 4d 43 54 3a 3a 6d 5f 54 72 61 6e 73 66 65 72
S 2564 16 0 0 0 222 1 582 5136 14 440000 A 0 0 0 0 0 0 0 0 2563 1103 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 2565 23 5 0 0 0 2569 582 14751 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 isend_
S 2566 1 3 1 0 243 1 2565 7709 14 3008 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 avin
S 2567 1 3 3 0 472 1 2565 9585 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2568 1 3 1 0 6 1 2565 14812 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2569 14 5 0 0 0 1 2565 14751 10 400000 A 0 0 0 0 0 0 0 584 3 0 0 0 0 0 0 0 0 0 0 0 0 103 0 582 0 0 0 0 isend_
F 2569 3 2566 2567 2568
S 2570 23 5 0 0 0 2572 582 14758 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 waitsend_
S 2571 1 3 3 0 472 1 2570 9585 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2572 14 5 0 0 0 1 2570 14758 10 400000 A 0 0 0 0 0 0 0 588 1 0 0 0 0 0 0 0 0 0 0 0 0 287 0 582 0 0 0 0 waitsend_
F 2572 1 2571
S 2573 23 5 0 0 0 2577 582 5859 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 send_
S 2574 1 3 1 0 243 1 2573 7552 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2575 1 3 3 0 472 1 2573 9585 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2576 1 3 1 0 6 1 2573 14812 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2577 14 5 0 0 0 1 2573 5859 10 400000 A 0 0 0 0 0 0 0 590 3 0 0 0 0 0 0 0 0 0 0 0 0 361 0 582 0 0 0 0 send_
F 2577 3 2574 2575 2576
S 2578 23 5 0 0 0 2583 582 14768 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 irecv_
S 2579 1 3 3 0 243 1 2578 7552 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2580 1 3 3 0 472 1 2578 9585 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2581 1 3 1 0 6 1 2578 14812 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2582 1 3 1 0 16 1 2578 2783 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sum
S 2583 14 5 0 0 0 1 2578 14768 10 400000 A 0 0 0 0 0 0 0 594 4 0 0 0 0 0 0 0 0 0 0 0 0 419 0 582 0 0 0 0 irecv_
F 2583 4 2579 2580 2581 2582
S 2584 23 5 0 0 0 2588 582 14775 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 waitrecv_
S 2585 1 3 3 0 243 1 2584 7552 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2586 1 3 3 0 472 1 2584 9585 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2587 1 3 1 0 16 1 2584 2783 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sum
S 2588 14 5 0 0 0 1 2584 14775 10 400000 A 0 0 0 0 0 0 0 599 3 0 0 0 0 0 0 0 0 0 0 0 0 583 0 582 0 0 0 0 waitrecv_
F 2588 3 2585 2586 2587
S 2589 23 5 0 0 0 2594 582 5865 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 recv_
S 2590 1 3 3 0 243 1 2589 7552 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 2591 1 3 3 0 472 1 2589 9585 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rout
S 2592 1 3 1 0 6 1 2589 14812 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 2593 1 3 1 0 16 1 2589 2783 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sum
S 2594 14 5 0 0 0 1 2589 5865 10 400000 A 0 0 0 0 0 0 0 603 4 0 0 0 0 0 0 0 0 0 0 0 0 790 0 582 0 0 0 0 recv_
F 2594 4 2590 2591 2592 2593
A 162 2 0 0 145 6 970 0 0 0 162 0 0 0 0 0 0 0 0 0
A 1013 2 0 0 800 6 1973 0 0 0 1013 0 0 0 0 0 0 0 0 0
A 1103 2 0 0 859 222 2563 0 0 0 1103 0 0 0 0 0 0 0 0 0
Z
Z
