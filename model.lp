/* Objective function */
max: +22.55 C1 +21.025 C2 +22.55 C3 +29.55 C4 +22.675 C5 +33.55 C6 +19.925 C7 +16.275 C8 +21.075 C9 +26.025 C10;

/* Constraints */
+C1 +C2 +C3 +C4 +C5 +C6 +C7 +C8 +C9 +C10 = 8;
0 C1 <= 50000;
R3: +C6 <= 2;
R4: +C2 <= 2;
+C1 +C7 +C10 <= 2;
+C3 +C4 +C5 <= 2;
+C8 +C9 <= 2;
0 C1 <= 2;

/* Variable bounds */
C1 <= 1;
C2 <= 1;
C3 <= 1;
C4 <= 1;
C5 <= 1;
C6 <= 1;
C7 <= 1;
C8 <= 1;
C9 <= 1;
C10 <= 1;

/* Integer definitions */
int C1,C2,C3,C4,C5,C6,C7,C8,C9,C10;
