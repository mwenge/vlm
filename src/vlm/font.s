myFont:
        dc.l $80008
        ; Each word is an index into the list of addresses below.
        dc.w 920, 1016, 1024, 1032 ; 0
        dc.w 1040, 280, 280, 280 ; 4
        dc.w 280, 280, 280, 280 ; 8
        dc.w 280, 280, 280, 280 ; 12
        dc.w 280, 280, 280, 280 ; 16
        dc.w 280, 280, 280, 280 ; 20
        dc.w 280, 280, 280, 280 ; 24
        dc.w 280, 280, 280, 280 ; 28
        dc.w 280, 304, 312, 320 ; 32
        dc.w 328, 336, 344, 352 ; 36
        dc.w 360, 368, 376, 384 ; 40
        dc.w 392, 400, 408, 416 ; 44
        dc.w 424, 432, 440, 448 ; 48
        dc.w 456, 464, 472, 480 ; 52
        dc.w 488, 496, 928, 280 ; 56
        dc.w 296, 280, 288, 280 ; 60
        dc.w 280, 712, 720, 728 ; 64
        dc.w 736, 744, 752, 760 ; 68
        dc.w 768, 776, 784, 792 ; 72
        dc.w 800, 808, 816, 824 ; 76
        dc.w 832, 840, 848, 856 ; 80
        dc.w 864, 872, 880, 888 ; 84
        dc.w 896, 904, 912, 280 ; 88
        dc.w 280, 280, 280, 280 ; 92
        dc.w 280, 504, 512, 520 ; 96
        dc.w 528, 536, 544, 552 ; 100
        dc.w 560, 568, 576, 584 ; 104
        dc.w 592, 600, 608, 616 ; 108
        dc.w 624, 632, 640, 648 ; 112
        dc.w 656, 664, 672, 680 ; 116
        dc.w 688, 696, 704, 280 ; 120
        dc.w 280, 280, 280, 280 ; 124
        dc.w 936, 944, 952, 960 ; 128
        dc.w 968, 976, 984, 992 ; 132
        dc.w 1000, 1008, 0, 0   ; 136
        dc.w 0, 0               ; 140

        ; A list of 'source' address for each font character.
        ; The list above addresses this table by index. For example '280'
        ; in the list above addresses the first entry here.
        dc.l $20100804, $04081020 ; 280
        dc.l $04081020, $20100804 ; 288
        dc.l $20202020, $20000020 ; 296
        dc.l $50505000, $00000000 ; 304
        dc.l $4848FE48, $48FE4848 ; 312
        dc.l $107E9070, $1014FE20 ; 320
        dc.l $60620408, $10204C8C ; 328
        dc.l $20508870, $64989A64 ; 336
        dc.l $00102040, $00000000 ; 344
        dc.l $10204040, $40402010 ; 352
        dc.l $20100808, $08081020 ; 360
        dc.l $00925438, $38549200 ; 368
        dc.l $101010FE, $10101000 ; 376
        dc.l $00000000, $00101020 ; 384
        dc.l $000000FE, $00000000 ; 392
        dc.l $00000000, $00606000 ; 400
        dc.l $00020408, $10204080 ; 408
        dc.l $7C82868A, $92A2C27C ; 416
        dc.l $10305010, $101010FE ; 424
        dc.l $78840408, $102040FE ; 432
        dc.l $7C820204, $7C02827C ; 440
        dc.l $040C1424, $4484FE04 ; 448
        dc.l $FE8080FC, $0202827C ; 456
        dc.l $384080FC, $8282827C ; 464
        dc.l $FE020408, $10204080 ; 472
        dc.l $7C82827C, $8282827C ; 480
        dc.l $7C828282, $7E02827C ; 488
        dc.l $0000007C, $027E827E ; 496
        dc.l $808080FC, $828282FC ; 504
        dc.l $0000007E, $8080807E ; 512
        dc.l $0202027E, $8282827E ; 520
        dc.l $0000007C, $82FC807C ; 528
        dc.l $003E4040, $FE404040 ; 536
        dc.l $0000007E, $827E027C ; 544
        dc.l $808080FC, $82828282 ; 552
        dc.l $00100030, $10101038 ; 560
        dc.l $00100010, $101010E0 ; 568
        dc.l $80808088, $90F08884 ; 576
        dc.l $60202020, $20202070 ; 584
        dc.l $0000007C, $92929282 ; 592
        dc.l $00000078, $84848484 ; 600
        dc.l $0000007C, $8282827C ; 608
        dc.l $000000FC, $82FC8080 ; 616
        dc.l $0000007E, $827E0202 ; 624
        dc.l $000000FC, $82808080 ; 632
        dc.l $0000007E, $403804FC ; 640
        dc.l $000010FE, $1010100C ; 648
        dc.l $00000082, $8282867A ; 656
        dc.l $00000082, $82442810 ; 664
        dc.l $00000082, $8292AAC6 ; 672
        dc.l $00000084, $48304884 ; 680
        dc.l $00000082, $827E02FC ; 688
        dc.l $000000FE, $041040FE ; 696
        dc.l $10284482, $FE828282 ; 704
        dc.l $FC8284FC, $828282FC ; 712
        dc.l $7EC08080, $8080C07E ; 720
        dc.l $FC868282, $828286FC ; 728
        dc.l $FE8080FE, $808080FE ; 736
        dc.l $FE8080FE, $80808080 ; 744
        dc.l $7EC08080, $8086C27C ; 752
        dc.l $828282FE, $82828282 ; 760
        dc.l $FE101010, $101010FE ; 768
        dc.l $FE080808, $08888870 ; 776
        dc.l $848890E0, $90888482 ; 784
        dc.l $80808080, $808080FE ; 792
        dc.l $82C6AA92, $82828282 ; 800
        dc.l $82C2A292, $8A868282 ; 808
        dc.l $7CC68282, $8282C67C ; 816
        dc.l $FC828282, $FC808080 ; 824
        dc.l $7CC68282, $828AC67A ; 832
        dc.l $FC828282, $FC888482 ; 840
        dc.l $7E80C030, $1C0602FC ; 848
        dc.l $FE101010, $10101010 ; 856
        dc.l $82828282, $8282827C ; 864
        dc.l $82828244, $44282810 ; 872
        dc.l $82828282, $8292AAC6 ; 880
        dc.l $82824438, $28448282 ; 888
        dc.l $82828244, $28101010 ; 896
        dc.l $FE020408, $102040FE ; 904
        dc.l $C0F0FCFE, $FEFCF0C0 ; 912
        dc.l $00003000, $00300000 ; 920
        dc.l $003C6666, $66663C00 ; 928
        dc.l $00381818, $18183C00 ; 936
        dc.l $00386C0C, $18307C00 ; 944
        dc.l $007E060C, $06663C00 ; 952
        dc.l $001C3C6C, $7E0C0C00 ; 960
        dc.l $007E607C, $06067C00 ; 968
        dc.l $003C607C, $66663C00 ; 976
        dc.l $003E060C, $18181800 ; 984
        dc.l $003C663C, $66663C00 ; 992
        dc.l $003C663E, $06063C00 ; 1000
        dc.l $FFFFFFFF, $FFFFFFFF ; 1008
        dc.l $FF8F8F8F, $8F8F8FFF ; 1016
        dc.l $FFF1F1F1, $F1F1F1FF ; 1024
        dc.l $FF818181, $818181FF ; 1032


; vim:ft=asm68k ts=2
