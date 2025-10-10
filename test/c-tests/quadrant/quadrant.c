// Checks if input points (x1,y1) and (x2,y2) lie in the same quadrant
// of the plane. Returns the quadrant number if so, otherwise returns 0.

int quadrant (int x1, int y1, int x2, int y2){
	if(x1 >= 0 && x2 >= 0 && y1 >= 0 && y2 >= 0)
		return 1; // (+,+): quadrant 1
	if(x1 <= 0 && x2 <= 0 && y1 >= 0 && y2 >= 0)
		return 2; // (-,+): quadrant 2
	if(x1 <= 0 && x2 <= 0 && y1 <= 0 && y2 <= 0)
		return 3; // (-,-): quadrant 3
	if(x1 >= 0 && x2 >= 0 && y1 <= 0 && y2 <= 0)
		return 4; // (+,-): quadrant 4
	return 0; // not in the same quadrant
 }
