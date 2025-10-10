short a;
int neg () {
  if (a > 0)
    return a;			/* trivially wrong code */
  else
    return a;			/* uncoverable with given fixtures */
}
