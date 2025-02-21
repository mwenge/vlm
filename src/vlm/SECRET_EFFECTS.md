# VLM: [Effects Hidden Since the Foundation of the Light Machine](https://en.wikipedia.org/wiki/Things_Hidden_Since_the_Foundation_of_the_World)

When VLM launches it selects `Bank 10` and  `Effect 5` to support the background display of its
'attract mode'. This is what it looks like in action:

[![Effect 5](https://github.com/user-attachments/assets/39491849-47e9-4879-8b1b-7465346fccd5)](https://www.youtube.com/watch?v=qJTREi4NCwY)

You can click on the picture above to view the full video on YouTube.

The other 8 effects in Bank 10 are inaccessible to the player once VLM is launched. This is because the VLM
only allows single-digit selections from 1 to 9.

This means some moderately interesting artefacts have been hidden from view, lying buried in 
`vlm.abs` since 1994. Eight VLM effects previously unknown to computerdom. Now that we have deconstructed 
the VLM binary and turned it into source code that can be
tinkered with and reassembled we have the opportunity to bring them back to the surface again.

To do this we twiddle the first line in the listing below (`move #5,skid`). For example, changing
it to `move #1,skid` to select Effect 1 in Bank 10.

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

.. we discover some buried treasure.

## Effect 1

[![Effect 1](https://github.com/user-attachments/assets/8a7b6d98-d0d5-40ff-b0e2-82c63af6ccf3)](https://www.youtube.com/watch?v=305jcrvpkqU)

## Effect 2

[![Effect 2](https://github.com/user-attachments/assets/bc2efd87-9946-4aeb-9538-f0f714adbad3)](https://www.youtube.com/watch?v=cPo-Yd92Uxg)

## Effect 3

[![Effect 3](https://github.com/user-attachments/assets/bd9d09d1-fbb3-4d7b-aad3-f8fd58dbcb00)](https://www.youtube.com/watch?v=nxgBADcgong)

## Effect 4

[![Effect 4](https://github.com/user-attachments/assets/7ec1547e-901b-4135-b856-972da4d3f614)](https://www.youtube.com/watch?v=iYFedyA8NNM)

## Effect 5

[![Effect 5](https://github.com/user-attachments/assets/a6b96b86-c089-4d4d-9e75-f1f5688d9412)](https://www.youtube.com/watch?v=qJTREi4NCwY)

## Effect 6

[![Effect 6](https://github.com/user-attachments/assets/ae40430a-ca05-402f-8409-d60c05abea59)](https://www.youtube.com/watch?v=M_2NZCb2xW0)

## Effect 7

[![Effect 7](https://github.com/user-attachments/assets/84a1cc9a-dfc4-4960-91c0-271f69e815ae)](https://www.youtube.com/watch?v=N56szSGV1ts)

## Effect 8

[![Effect 8](https://github.com/user-attachments/assets/05c222f4-0fb3-4ef4-ba59-d3ff91ed7f63)](https://www.youtube.com/watch?v=uww-Cvj3VKM)

## Effect 9

[![Effect 9](https://github.com/user-attachments/assets/a2411dcb-f390-4534-add3-b2b728fc45ba)](https://www.youtube.com/watch?v=MAt_qdciuG4)


