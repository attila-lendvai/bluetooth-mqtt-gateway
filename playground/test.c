#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/ioctl.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>

void main(void)
{
  printf("started...\n");

  printf("HCIDEVDOWN is %x, HCIDEVRESET is %x...\n", HCIDEVDOWN, HCIDEVRESET);

  const char* mac = "00:1A:7D:DA:71:13";
  bdaddr_t device_address;

  str2ba(mac, &device_address);

  int device_id = hci_devid(mac);
  int socket = hci_open_dev(device_id);

  printf("hci open, socket is %d...\n", socket);

  if (hci_le_set_scan_parameters(socket, 0x01, htobs(0x0010), htobs(0x0010), 0x00, 0x00, 1000) < 0)
      printf("hci_le_set_scan_parameters, error code, %d - %s...\n", errno, strerror(errno));

  printf("done...\n");
}
