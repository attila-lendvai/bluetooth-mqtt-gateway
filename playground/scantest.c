#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <curses.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>

void hexDump (char *desc, void *addr, int len) {
    int i;
    unsigned char buff[17];
    unsigned char *pc = (unsigned char*)addr;

    // Output description if given.
    if (desc != NULL)
        printf ("%s:\n", desc);

    // Process every byte in the data.
    for (i = 0; i < len; i++) {
        // Multiple of 16 means new line (with line offset).

        if ((i % 16) == 0) {
            // Just don't print ASCII for the zeroth line.
            if (i != 0)
                printf ("  %s\n", buff);

            // Output the offset.
            printf ("  %04x ", i);
        }

        // Now the hex code for the specific character.
        printf (" %02x", pc[i]);

        // And store a printable ASCII character for later.
        if ((pc[i] < 0x20) || (pc[i] > 0x7e))
            buff[i % 16] = '.';
        else
            buff[i % 16] = pc[i];
        buff[(i % 16) + 1] = '\0';
    }

    // Pad out last line if not exactly 16 characters.
    while ((i % 16) != 0) {
        printf ("   ");
        i++;
    }

    // And print the final ASCII bit.
    printf ("  %s\n", buff);
}

void main(void)
{
  const char *mac = "00:1A:7D:DA:71:13";
  //struct dbaddr_t device_address;
  //str2ba(mac, &device_address);
  int device_id = hci_devid(mac);

  {
      // reset
      int ctl;

      printf("AF_BLUETOOTH %d, SOCK_RAW %d, BTPROTO_HCI %d, HCIDEVDOWN %x, HCIDEVUP %x\n", AF_BLUETOOTH, SOCK_RAW, BTPROTO_HCI, HCIDEVDOWN, HCIDEVUP);
      if ((ctl = socket(AF_BLUETOOTH, SOCK_RAW, BTPROTO_HCI)) < 0) {
          perror("Can't open HCI socket.");
          exit(1);
      }

      // down
      if (ioctl(ctl, HCIDEVDOWN, device_id) < 0) {
          fprintf(stderr, "Can't down device hci%d: %s (%d)\n",
                  device_id, strerror(errno), errno);
          exit(1);
      }
      // up
      if (ioctl(ctl, HCIDEVUP, device_id) < 0) {
          if (errno == EALREADY)
              return;
          fprintf(stderr, "Can't init device hci%d: %s (%d)\n",
                  device_id, strerror(errno), errno);
          exit(1);
      }
  }

  int socket = hci_open_dev(device_id);

  if (hci_le_set_scan_parameters(socket, 0x01, 0x0010, 0x0010, 0x00, 0x00, 1000) < 0)
  {
    printf("Failed to set scan parameters: %s\n", strerror(errno));
    return;
  }

  if (hci_le_set_scan_enable(socket, 0x01, 1, 1000) < 0)
  {
    printf("Failed to enable scan: %s\n", strerror(errno));
    return;
  }

  {
      int current_flags = fcntl(socket, F_GETFL);
      fcntl(socket, F_SETFL, current_flags | O_NONBLOCK);
  }

  /*
  int on = 1;
  if(ioctl(socket, FIONBIO, (char *)&on) < 0)
  {
    printf("Could set device to non-blocking: %s", strerror(errno));
    return;
  }
  */

  {
      struct hci_filter original_filter;
      socklen_t filter_struct_size = sizeof(struct hci_filter);
      getsockopt(socket, SOL_HCI, HCI_FILTER, &original_filter, &filter_struct_size);

      struct hci_filter new_filter;
      memset(&new_filter, 0, sizeof(struct hci_filter));
      //hci_filter_clear(&new_filter);
      hci_filter_set_ptype(HCI_EVENT_PKT, &new_filter);
      hci_filter_set_event(EVT_LE_META_EVENT, &new_filter);

      hexDump("new_filter:", &new_filter, (sizeof new_filter));
      printf("SOL_HCI %d, HCI_FILTER %d\n", SOL_HCI, HCI_FILTER);

      if (setsockopt(socket, SOL_HCI, HCI_FILTER, &new_filter, sizeof(new_filter)) < 0)
      {
          printf("Could not set socket options: %s\n", strerror(errno));
          return;
      }

      int done = FALSE;
      int error = FALSE;
      while(!done && !error)
      {
          int len = 0;
          unsigned char buf[HCI_MAX_EVENT_SIZE];
          while((len = read(socket, buf, sizeof(buf))) < 0)
          {
              printf("tick\n");

              if (errno == EINTR)
              {
                  done = TRUE;
                  break;
              }

              if (errno == EAGAIN || errno == EINTR)
              {
                  if(getch() == 'q')
                  {
                      done = TRUE;
                      break;
                  }

                  sleep(1);
                  continue;
              }

              error = TRUE;
          }

          if(!done && !error)
          {
              printf("Got %d bytes\n", len);
          }
      }

      printf("closing\n");
      hci_close_dev(socket);
      printf("done\n");
  }
}
