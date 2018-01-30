#include <iostream>
#include <getopt.h>
#include <string.h>
using namespace std;

static int length_flag;
static int help_flag;

int main(int argc, char *argv[]) {
  const struct option flags[] = {
    {"length", no_argument, &length_flag, 1},
    {"help",   no_argument, &help_flag, 1},
    {0, 0, 0, 0}
  };

  while (getopt_long_only(argc, argv, "", flags, NULL) != -1);

  if (help_flag) {
    cout << "Usage: cppiler [flags] [args]\n";
    cout << "Available flags:\n";
    cout << "\t-length\tprints the lengths of each of the arguments\n";
    cout << "\t-help\tprints this help message\n";
  } else if (length_flag) {
    while (optind < argc)
      printf ("%zd\n", strlen(argv[optind++]));
  } else if (optind < argc) {
    while (optind < argc)
      printf ("%s\n", argv[optind++]);
  }
}
