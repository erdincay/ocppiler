#include <iostream>
#include <getopt.h>
#include <string.h>
#include <unistd.h>
using namespace std;

int main(int argc, char *argv[]) {

  const struct option flags[] = {
    {"length", no_argument, 0, 'l'},
    {"help",   no_argument, 0, 'h'},
    {0,        0,           0, 0}
  };

  int flag = 0;
  while ((flag = getopt_long_only(argc, argv, "", flags, NULL)) != -1) {
    switch (flag) {
      case 'l':
        for (int i = 1; i<argc; i++) {
          cout << strlen(argv[i]) << "\n";
        }
        break;
      case 'h':
        cout << "Usage: cppiler [flags] [args]\n";
        cout << "Available flags:\n";
        cout << "\t-length: prints the lengths of each of the arguments\n";
        cout << "\t-help:   prints this help message\n";
        break;
      default:
        /* Echoes the command-line arguments given to the program back to the
        user, one argument per line. */
        for (int i = 1; i<argc; i++) {
          cout << argv[i] << "\n";
        }
    }
  }
}
