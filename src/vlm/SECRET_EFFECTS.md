# VLM: [Effects Hidden Since the Foundation of the Light Machine](https://en.wikipedia.org/wiki/Things_Hidden_Since_the_Foundation_of_the_World)

When VLM launches it selects `Bank 10` and  `Effect 5` to support the background display of its
'attract mode'. This is what it looks like in action:

[![Effect 5](https://img.youtube.com/vi/qJTREi4NCwY/0.jpg)](https://www.youtube.com/watch?v=qJTREi4NCwY)

You can click on the picture above to view the full video on YouTube.

The other 8 effects in Bank 10 are inaccessible to the player once VLM is launched. This is because the VLM
only allows single-digit selections from 1 to 9:

This means some moderately interesting artefacts have been hidden from view, lying buried in 
`vlm.abs`. Eight VLM effects previously unknown to computerdom. Now that we have deconstructed 
the VLM binary and turned it into source code that can be
tinkered with and reassembled we have the opportunity to bring them back to the surface again.

To do this we twiddle the first line in the listing below (`move    #5,skid`). For example, changing
it to `move    #1,skid` to select Effect 1 in Bank 10.

```asm
; *******************************************************************
; LaunchVLM
;
; This is where execution starts. Set the initial Bank and Effect to
; 9-5 and enter the main loop.
; *******************************************************************
free:
LaunchVLM:
        move    #5,skid          ; <-- CHANGE '5' TO '1' TO '9' TO GET A NEW EFFECT!
        movea.l #stack,sp        ; Set 'sp' as our stack pointer.
        move.l  #rrts,davesvec
        move    #0,started       ; Indicate we haven't started the VLM yet.
        move    #1,freerun       ; Displaying 'Jaguar' 'wait mode'
        move    #9,imatrix       ; Set Bank Number to 10 (9+1)
        bsr.w   everything       ; Set up everything.
        lea     davesobj,a0      ; Who's Dave?
        rts
```

When we reassemble and run:

```
make
bigpemu src/bin/vlm.abs
```

.. we discover new treasures.

## Effect 1

[![Effect 1](https://img.youtube.com/vi/305jcrvpkqU/0.jpg)](https://www.youtube.com/watch?v=305jcrvpkqU)

## Effect 2

[![Effect 2](https://img.youtube.com/vi/cPo-Yd92Uxg/0.jpg)](https://www.youtube.com/watch?v=cPo-Yd92Uxg)

## Effect 3

[![Effect 3](https://img.youtube.com/vi/nxgBADcgong/0.jpg)](https://www.youtube.com/watch?v=nxgBADcgong)

## Effect 4

[![Effect 4](https://img.youtube.com/vi/iYFedyA8NNM/0.jpg)](https://www.youtube.com/watch?v=iYFedyA8NNM)

## Effect 5

[![Effect 5](https://img.youtube.com/vi/qJTREi4NCwY/0.jpg)](https://www.youtube.com/watch?v=qJTREi4NCwY)

## Effect 6

[![Effect 6](https://img.youtube.com/vi/M_2NZCb2xW0/0.jpg)](https://www.youtube.com/watch?v=M_2NZCb2xW0)

## Effect 7

[![Effect 7](https://img.youtube.com/vi/N56szSGV1ts/0.jpg)](https://www.youtube.com/watch?v=N56szSGV1ts)

## Effect 8

[![Effect 8](https://img.youtube.com/vi/uww-Cvj3VKM/0.jpg)](https://www.youtube.com/watch?v=uww-Cvj3VKM)

## Effect 9

[![Effect 9](https://img.youtube.com/vi/MAt_qdciuG4/0.jpg)](https://www.youtube.com/watch?v=MAt_qdciuG4)


