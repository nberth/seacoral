/* Should copy all the elements
   of ordered arrays t1 and t2
   of lengths l1 and l2
   into the ordered array t3

   Contains a lot of infeasible paths
*/

   void merge (int t1[10], int t2[10], int t3[20]) {

	int i = 0;
	int j = 0;
	int k = 0;

	while (i < 10 && j < 10) {
		if (t1[i] < t2[j]) {
			t3[k] = t1[i];
			i++;
		}
		else {
			t3[k] = t2[j];
			j++;
		}
		k++;
	}

	while (i < 10) {
		t3[k] = t1[i];
		i++;
		k++;
	}

	while (j < 10) {
		t3[k] = t2[j];
		j++;
		k++;
	}
}
