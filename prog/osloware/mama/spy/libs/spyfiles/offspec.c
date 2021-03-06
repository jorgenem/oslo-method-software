#include        <stdio.h>
#include        <errno.h>
#include        <sys/file.h>
#include        <sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include        <fcntl.h>
#include	<signal.h>
#include	<sys/shm.h>
#include	<sys/time.h>

#include	</user/schiller/osloware/include/offmem_defs.h>
#include	</user/schiller/osloware/include/offspec_defs.h>

int *offspec( int specno )
{


	struct shmid_ds  *mbuf;
        struct shminfo   *inf; 
	int		 shmid;
    	int		 shmemsize;
        int              *shmptr;
	key_t		 mkey;


	if ( specno == 1) {			/* Attach singles area */
	   mkey = SINGLES_KEY;
	   shmemsize = SINGLES_SIZE;
        }
	if ( specno == 2) {			/* Attach E-spectra */
	   mkey = ESP_KEY;
	   shmemsize = ESP_SIZE;
        }
	if ( specno == 3) {			/* Attach DE-spectra */
	   mkey = DESP_KEY;
	   shmemsize = DESP_SIZE;
        }
	if ( specno == 4) {			/* Attach DE-E-spectra */
	   mkey = EDESP_KEY;
	   shmemsize = EDESP_SIZE;
        }
	if ( specno == 5) {			/* Attach Thickness spectra */
	   mkey = THICKSP_KEY;
	   shmemsize = THICKSP_SIZE;
        }
	if ( specno == 6) {			/* Attach Ge-spectra */
	   mkey = GESP_KEY;
	   shmemsize = GESP_SIZE;
        }
	if ( specno == 7) {			/* Attach Ge-T-spectra */
	   mkey = TGESP_KEY;
	   shmemsize = TGESP_SIZE;
        }
	if ( specno == 8) {			/* Attach NaI-spectra */
	   mkey = NASP_KEY;
	   shmemsize = NASP_SIZE;
        }
	if ( specno == 9) {			/* Attach NaI-T-spectra */
	   mkey = TNASP_KEY;
	   shmemsize = TNASP_SIZE;
        }
	if ( specno == 10) {			/* Attach Alpha-NaI-spectra */
	   mkey = ALFNA_KEY;
	   shmemsize = ALFNA_SIZE;
        }
	if ( specno == 11) {			/* Attach Alpha-Ge-spectra */
	   mkey = ALFGE_KEY;
	   shmemsize = ALFGE_SIZE;
        }
	if ( specno == 12) {			/* Attach GP-spectra */
	   mkey = MAT_KEY;
	   shmemsize = MAT_SIZE;
        }


       /* Attach shared memory to process */
	shmid = shmget(mkey, shmemsize, PERMS);		
    	if (shmid == -1) {
      	   perror("\n **** ERROR ****  Shared Memory Allocation Failed  \n");
           return NULL;
        }
        shmptr = shmat(shmid, NULL, 0);			
        if (*shmptr == -1) {
           perror("\n **** ERROR **** Attach segment to process failed  \n");
           return NULL;
        } 

	return shmptr;

}
