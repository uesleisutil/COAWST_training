V26 m_accumulatorcomms
22 m_AccumulatorComms.F90 S582 0
03/26/2019  11:41:33
use m_attrvect private
use m_list private
use m_string private
use m_globalmap private
use m_globalsegmap private
use m_accumulator private
use m_attrvect private
use m_list private
use m_string private
use m_globalmap private
use m_globalsegmap private
use m_accumulator private
enduse
D 56 18 12
D 58 24 604 192 603 7
D 334 24 1391 808 1390 7
D 433 24 604 192 603 7
D 439 24 1391 808 1390 7
D 445 24 1651 280 1648 7
D 557 24 1651 280 1648 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 29 0 0 0 0 0 0 m_accumulatorcomms
S 583 19 0 0 0 8 1 582 4677 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 2 0 0 0 0 0 582 0 0 0 0 gather
O 583 2 587 586
S 584 19 0 0 0 8 1 582 4684 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 2 0 0 0 0 0 582 0 0 0 0 scatter
O 584 2 589 588
S 585 19 0 0 0 8 1 582 4692 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 1 0 0 0 0 0 582 0 0 0 0 bcast
O 585 1 590
S 586 27 0 0 0 8 1638 582 4698 10010 400000 A 0 0 0 0 0 0 253 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_gather_
Q 586 583 0
S 587 27 0 0 0 8 1937 582 4709 10010 400000 A 0 0 0 0 0 0 306 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_gather_
Q 587 583 0
S 588 27 0 0 0 8 1945 582 4721 10010 400000 A 0 0 0 0 0 0 307 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_scatter_
Q 588 584 0
S 589 27 0 0 0 8 1953 582 4733 10010 400000 A 0 0 0 0 0 0 308 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_scatter_
Q 589 584 0
S 590 27 0 0 0 8 1961 582 4746 10010 400000 A 0 0 0 0 0 0 309 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 bcast_
Q 590 585 0
S 591 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 592 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 4753 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 23 4d 43 54 3a 3a 6d 5f 41 63 63 75 6d 75 6c 61 74 6f 72 43 6f 6d 6d 73
S 593 16 0 0 0 56 1 582 4777 14 440000 A 0 0 0 0 0 0 0 0 592 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 603 25 1 m_globalmap globalmap
R 604 5 2 m_globalmap comp_id globalmap
R 605 5 3 m_globalmap gsize globalmap
R 606 5 4 m_globalmap lsize globalmap
R 608 5 6 m_globalmap counts globalmap
R 609 5 7 m_globalmap counts$sd globalmap
R 610 5 8 m_globalmap counts$p globalmap
R 611 5 9 m_globalmap counts$o globalmap
R 614 5 12 m_globalmap displs globalmap
R 615 5 13 m_globalmap displs$sd globalmap
R 616 5 14 m_globalmap displs$p globalmap
R 617 5 15 m_globalmap displs$o globalmap
R 619 19 17 m_globalmap gsize
R 620 19 18 m_globalmap lsize
R 621 19 19 m_globalmap init
R 622 19 20 m_globalmap init_remote
R 623 19 21 m_globalmap clean
R 624 19 22 m_globalmap rank
R 625 19 23 m_globalmap bounds
R 626 19 24 m_globalmap comp_id
R 705 19 8 m_string tochar
R 706 19 9 m_string string_init
R 707 19 10 m_string init
R 708 19 11 m_string string_clean
R 709 19 12 m_string clean
R 710 19 13 m_string string_len
R 711 19 14 m_string string_bcast
R 712 19 15 m_string bcast
R 713 19 16 m_string string_mci
R 714 19 17 m_string string_mco
R 715 19 18 m_string ptr_chars
R 716 19 19 m_string char
R 724 19 27 m_string len
R 878 19 15 m_list init
R 879 19 16 m_list clean
R 880 19 17 m_list nullify
R 881 19 18 m_list get_indices
R 882 19 19 m_list test_indices
R 883 19 20 m_list nitem
R 884 19 21 m_list get
R 885 19 22 m_list identical
R 887 19 24 m_list copy
R 888 19 25 m_list exporttochar
R 889 19 26 m_list exporttostring
R 890 19 27 m_list charbuffersize
R 891 19 28 m_list append
R 892 19 29 m_list concatenate
R 893 19 30 m_list bcast
R 894 19 31 m_list send
R 895 19 32 m_list recv
R 896 19 33 m_list getsharedlistindices
R 902 19 39 m_list index
R 913 19 50 m_list allocated
R 1092 19 22 m_attrvect init
R 1093 19 23 m_attrvect clean
R 1094 19 24 m_attrvect zero
R 1095 19 25 m_attrvect lsize
R 1096 19 26 m_attrvect niattr
R 1097 19 27 m_attrvect nrattr
R 1098 19 28 m_attrvect indexia
R 1099 19 29 m_attrvect indexra
R 1100 19 30 m_attrvect getilist
R 1101 19 31 m_attrvect getrlist
R 1102 19 32 m_attrvect exportilist
R 1103 19 33 m_attrvect exportrlist
R 1104 19 34 m_attrvect exportilisttochar
R 1105 19 35 m_attrvect exportrlisttochar
R 1106 19 36 m_attrvect appendiattr
R 1107 19 37 m_attrvect appendrattr
R 1108 19 38 m_attrvect exportiattr
R 1109 19 39 m_attrvect exportrattr
R 1110 19 40 m_attrvect importiattr
R 1111 19 41 m_attrvect importrattr
R 1112 19 42 m_attrvect copy
R 1113 19 43 m_attrvect rcopy
R 1114 19 44 m_attrvect icopy
R 1115 19 45 m_attrvect sort
R 1116 19 46 m_attrvect permute
R 1117 19 47 m_attrvect unpermute
R 1118 19 48 m_attrvect sortpermute
R 1119 19 49 m_attrvect sharedattrindexlist
R 1390 25 6 m_accumulator accumulator
R 1391 5 7 m_accumulator num_steps accumulator
R 1392 5 8 m_accumulator steps_done accumulator
R 1394 5 10 m_accumulator iaction accumulator
R 1395 5 11 m_accumulator iaction$sd accumulator
R 1396 5 12 m_accumulator iaction$p accumulator
R 1397 5 13 m_accumulator iaction$o accumulator
R 1400 5 16 m_accumulator raction accumulator
R 1401 5 17 m_accumulator raction$sd accumulator
R 1402 5 18 m_accumulator raction$p accumulator
R 1403 5 19 m_accumulator raction$o accumulator
R 1405 5 21 m_accumulator data accumulator
R 1406 19 22 m_accumulator init
R 1407 19 23 m_accumulator initp
R 1408 19 24 m_accumulator clean
R 1409 19 25 m_accumulator initialized
R 1410 19 26 m_accumulator lsize
R 1411 19 27 m_accumulator numsteps
R 1412 19 28 m_accumulator stepsdone
R 1413 19 29 m_accumulator niattr
R 1414 19 30 m_accumulator nrattr
R 1415 19 31 m_accumulator indexia
R 1416 19 32 m_accumulator indexra
R 1417 19 33 m_accumulator getilist
R 1418 19 34 m_accumulator getrlist
R 1419 19 35 m_accumulator exportiattr
R 1420 19 36 m_accumulator exportrattr
R 1421 19 37 m_accumulator importiattr
R 1422 19 38 m_accumulator importrattr
R 1423 19 39 m_accumulator zero
R 1424 19 40 m_accumulator sharedattrindexlist
R 1425 19 41 m_accumulator accumulate
R 1426 19 42 m_accumulator average
S 1638 23 5 0 0 0 1645 582 4698 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_gather_
S 1639 1 3 1 0 439 1 1638 8292 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1640 1 3 2 0 439 1 1638 8295 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1641 1 3 1 0 433 1 1638 5099 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1642 1 3 1 0 6 1 1638 5116 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1643 1 3 1 0 6 1 1638 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1644 1 3 2 0 6 1 1638 5211 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1645 14 5 0 0 0 1 1638 4698 10 400000 A 0 0 0 0 0 0 0 449 6 0 0 0 0 0 0 0 0 0 0 0 0 95 0 582 0 0 0 0 gm_gather_
F 1645 6 1639 1640 1641 1642 1643 1644
R 1648 25 1 m_globalsegmap globalsegmap
R 1649 19 2 m_globalsegmap init
R 1650 19 3 m_globalsegmap clean
R 1651 5 4 m_globalsegmap comp_id globalsegmap
R 1652 5 5 m_globalsegmap gsize globalsegmap
R 1653 19 6 m_globalsegmap globalstorage
R 1654 19 7 m_globalsegmap processstorage
R 1655 19 8 m_globalsegmap orderedpoints
R 1656 19 9 m_globalsegmap lsize
R 1657 5 10 m_globalsegmap ngseg globalsegmap
R 1658 19 11 m_globalsegmap nlseg
R 1659 19 12 m_globalsegmap max_nlseg
R 1660 19 13 m_globalsegmap active_pes
R 1661 19 14 m_globalsegmap pelocs
R 1662 19 15 m_globalsegmap haloed
R 1663 19 16 m_globalsegmap rank
R 1664 19 17 m_globalsegmap sort
R 1665 19 18 m_globalsegmap permute
R 1666 19 19 m_globalsegmap sortpermute
R 1667 19 20 m_globalsegmap increasing
R 1668 19 21 m_globalsegmap copy
R 1670 5 23 m_globalsegmap start globalsegmap
R 1671 5 24 m_globalsegmap start$sd globalsegmap
R 1672 5 25 m_globalsegmap start$p globalsegmap
R 1673 5 26 m_globalsegmap start$o globalsegmap
R 1676 5 29 m_globalsegmap length globalsegmap
R 1677 5 30 m_globalsegmap length$sd globalsegmap
R 1678 5 31 m_globalsegmap length$p globalsegmap
R 1679 5 32 m_globalsegmap length$o globalsegmap
R 1682 5 35 m_globalsegmap pe_loc globalsegmap
R 1683 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1684 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1685 5 38 m_globalsegmap pe_loc$o globalsegmap
R 1694 19 47 m_globalsegmap comp_id
R 1696 19 49 m_globalsegmap gsize
R 1702 19 55 m_globalsegmap ngseg
S 1937 23 5 0 0 0 1944 582 4709 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_gather_
S 1938 1 3 1 0 334 1 1937 8292 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1939 1 3 2 0 334 1 1937 8295 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1940 1 3 1 0 557 1 1937 8815 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1941 1 3 1 0 6 1 1937 5116 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1942 1 3 1 0 6 1 1937 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1943 1 3 2 0 6 1 1937 5211 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1944 14 5 0 0 0 1 1937 4709 10 400000 A 0 0 0 0 0 0 0 565 6 0 0 0 0 0 0 0 0 0 0 0 0 199 0 582 0 0 0 0 gsm_gather_
F 1944 6 1938 1939 1940 1941 1942 1943
S 1945 23 5 0 0 0 1952 582 4721 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_scatter_
S 1946 1 3 1 0 334 1 1945 8292 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1947 1 3 2 0 334 1 1945 8295 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1948 1 3 1 0 58 1 1945 5099 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1949 1 3 1 0 6 1 1945 5116 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1950 1 3 1 0 6 1 1945 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1951 1 3 2 0 6 1 1945 5211 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1952 14 5 0 0 0 1 1945 4721 10 400000 A 0 0 0 0 0 0 0 572 6 0 0 0 0 0 0 0 0 0 0 0 0 299 0 582 0 0 0 0 gm_scatter_
F 1952 6 1946 1947 1948 1949 1950 1951
S 1953 23 5 0 0 0 1960 582 4733 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_scatter_
S 1954 1 3 1 0 334 1 1953 8292 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ic
S 1955 1 3 2 0 334 1 1953 8295 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oc
S 1956 1 3 1 0 445 1 1953 8815 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1957 1 3 1 0 6 1 1953 5116 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1958 1 3 1 0 6 1 1953 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1959 1 3 2 0 6 1 1953 5211 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1960 14 5 0 0 0 1 1953 4733 10 400000 A 0 0 0 0 0 0 0 579 6 0 0 0 0 0 0 0 0 0 0 0 0 407 0 582 0 0 0 0 gsm_scatter_
F 1960 6 1954 1955 1956 1957 1958 1959
S 1961 23 5 0 0 0 1966 582 4746 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcast_
S 1962 1 3 3 0 334 1 1961 8070 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ac
S 1963 1 3 1 0 6 1 1961 5116 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1964 1 3 1 0 6 1 1961 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1965 1 3 2 0 6 1 1961 5211 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1966 14 5 0 0 0 1 1961 4746 10 400000 A 0 0 0 0 0 0 0 586 4 0 0 0 0 0 0 0 0 0 0 0 0 509 0 582 0 0 0 0 bcast_
F 1966 4 1962 1963 1964 1965
S 1967 23 5 0 0 0 1972 582 9111 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcastp_
S 1968 1 3 3 0 334 1 1967 8070 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ac
S 1969 1 3 1 0 6 1 1967 5116 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1970 1 3 1 0 6 1 1967 5107 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1971 1 3 2 0 6 1 1967 5211 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1972 14 5 0 0 0 1 1967 9111 10 400000 A 0 0 0 0 0 0 0 591 4 0 0 0 0 0 0 0 0 0 0 0 0 601 0 582 0 0 0 0 bcastp_
F 1972 4 1968 1969 1970 1971
A 12 2 0 0 0 6 591 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 56 592 0 0 0 13 0 0 0 0 0 0 0 0 0
Z
Z
