#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// This defines the matrix data structure
typedef struct
{
	double **matrix;
	// This is the 2D array that will be our matrix
	int rows,columns;
	// These integers store the shape of the array
}Matrix;

// When this function is called and assigned to a matrix pointer it allocates memory for a matrix
Matrix *matrix(int rows,int columns)
{
	Matrix *mtrx = (Matrix*)malloc(sizeof(Matrix));
	// Space for matrix is being allocated
	mtrx->rows = rows;
	// Assigns number of rows in matrix
	mtrx->columns = columns;
	// Assigns number of columns in matrix
	mtrx->matrix = (double**)malloc(sizeof(double*)*rows);
	// Allocates memory for rows in matrix
	int i;
	for(i = 0; i < rows; i++){
		mtrx->matrix[i] = (double*)malloc(sizeof(double)*columns);
		// Allocates memory for columns in matrix
	}
	return mtrx;
	// Returns matrix
}

// This function prints values stored in matrices
void print(Matrix* matrix)
{
	int i,j;
	for(i = 0; i < matrix->rows; i++){
		for(j = 0; j < matrix->columns;j++)
			printf("%.lf ",matrix->matrix[i][j]);
			// Prints values stored in matrix
		printf("\n");
		// Prints new line to let user know of new row
	}
}

// This function stores random values in matrices
void random_mtrx(Matrix* matrix)
{
	srand(time(NULL));
	// Randomizes seed
	int i,j;
	for(i = 0; i < matrix->rows; i++){
		for(j = 0; j < matrix->columns;j++){
			int value = rand()%100 + 1;
			// Stores random integer value that will be stored in matrix as a double
			matrix->matrix[i][j] = (double)value;
			// Stores random value in matrix 
		}
	}	
}

// This function prints the shape of a matrix
void mtrx_shape(Matrix *matrix)
{
	printf("(%d,%d)\n",matrix->rows,matrix->columns);
	// Prints shape of matrix	
}

// This function adds matrices
Matrix *mtrx_sum(Matrix m1,Matrix m2)
{
	if(m1.rows == m2.rows && m1.columns == m2.columns)
	{
		// Checks if matrices are of the same shape
		Matrix *sum = matrix(m1.rows,m1.columns);
		// Creates a new matrix of the same shape as the two matrices that are being added
		int i,j;
		for(i = 0; i < m1.rows; i++)
		{
			for(j = 0; j < m1.columns; j++)
			{
				sum->matrix[i][j] = m1.matrix[i][j] + m2.matrix[i][j];
				// Add two values stored in matrices and stores value in new matrix
			}
		}
		return sum;
		// Returns sum of matrices
	}
	else
	{
		printf("Matrices of different shapes cannot be added\n");
		// Error message to let user know that matrices cannot be added
		return NULL;
		// Returns null if matrices cannot be addedd
	}
}

// This function subtracts matrices
Matrix *mtrx_sub(Matrix m1,Matrix m2)
{
	if(m1.rows == m2.rows && m1.columns == m2.columns)
	{
		// Checks if matrices are of the same shape
		Matrix *diff = matrix(m1.rows,m1.columns);
		// Creates a new matrix of the same shape as the two matrices that are being subtracted
		int i,j;
		for(i = 0; i < m1.rows; i++)
		{
			for(j = 0; j < m1.columns; j++)
				diff->matrix[i][j] = m1.matrix[i][j] - m2.matrix[i][j];
				// Subtracts two values stored in matrices and stores value in the new matrix
		}
		return diff;
		// Returns subtraction of matrices
	}
	else
	{
		printf("Matrices of different shapes cannot be subtracted\n");
		// Error message lets the user know that matrices cannot be subtracted
		return NULL;
		// Returns null if matrices cannot be subtracted
	}
}

// This function performs a dot product 
Matrix *dot(Matrix m1,Matrix m2)
{
	if(m1.columns == m2.rows)
	{
		// Checks if the columns of the first matrix is the same as the number of rows in the second matrix to see if it is possible to get a dot product
		Matrix *prod = matrix(m1.rows,m2.columns);
		//Creates new matrix for matrix multiplication
		int i,j,k;
		double sum = 0;
		// Initialize product of every entry to zero
		
		// Dot Product procedure
		for(i = 0;i < m1.rows;i++)
		{
			for(j = 0; j < m2.columns; j++)
			{
				for(k = 0; k < m2.rows; k++)
				{
					sum += m1.matrix[i][k] * m2.matrix[k][j];
				}
				prod->matrix[i][j] = sum;
				sum = 0;
			}
		}
		return prod;
		// Returns dot product of matrices
	}
	else
	{
		printf("Matrices of shape (%d,%d) and shape (%d,%d) cannot be multiplied\n",m1.rows,m1.columns,m2.rows,m2.columns);
		// Error message lets the user know that the matrices cannot be multiplied
		return NULL;
		// Returns null if the matrices cannot be multiplied
	}
}

// This function returns the determinant of a matrix
double det(Matrix m)
{
	if(m.rows != m.columns)
	{
		// Checks if matrix is a square matrix
		printf("Error calculating Determinant : Matrix must be a square matrix for determinant to be calculated\n");
		return 0.00;
	}
	else if(m.rows == 2)
	{
		// Performs operation for finding the determinant of a 2x2 matrix 
		return m.matrix[0][0]*m.matrix[1][1] - m.matrix[0][1] * m.matrix[1][0];
	}
	else
	{
		double deter = 0;
		// Initializes determinant at 0
		int i,j,k;
		for(i = 0; i < m.rows; i++)
		{
			// Creates smaller matrix for calcuating Minor
			Matrix *m1 = matrix(m.rows-1,m.rows-1);
			int counter = 0;
			for(j = 0; j < m1->rows; j++)
			{
				for(k = 0; k < m1->rows; k++)
				{
					if(k == i)counter = 1;
					// Passes elements from matrix to smaller matrix for computing minor
					m1->matrix[j][k] = m.matrix[j+1][k+counter];
				}
				counter = 0;
			}
			if(i%2 == 0)
			{
				deter += m.matrix[0][i] * det(*m1);
			}
			else
			{
				deter -= m.matrix[0][i]*det(*m1);
			}
		}
		// Returns determinant
		return deter;
	}
}

// Function gets cofactor of matrix
void get_cofactor(Matrix *m1, Matrix *temp, int p, int q, int n){
	int i = 0,j = 0,row,column;
	for(row = 0; row < m1->rows;row++){
		for(column = 0; column < m1->columns; column++){
			if(row != p && column != q){
				temp->matrix[i][j++] = m1->matrix[row][column];
				if(j == n - 1){
					j = 0;
					i++;
				}		
			}
		}
	}
}

// Function returns adjoint of a matrix
Matrix *adjoint(Matrix *m1){
	if(m1->rows != m1->columns){
		return NULL;
	} else{
		int sign = 1,i,j;
		Matrix *temp = matrix(m1->rows,m1->columns);
		Matrix *adj = matrix(m1->rows,m1->columns);
	       	for(i = 0; i < m1->rows; i++){
			for(j = 0; j < m1->columns; j++){
				get_cofactor(m1,temp,i,j,m1->rows);
				sign =((i+j)%2 == 0)? 1: -1;
				
				adj->matrix[j][i] = (sign)*det(*temp);
			}
		}
		return adj;
	}
}

// Main program
void main(){
	Matrix *mtrx1 = matrix(1,3);
	Matrix *mtrx2 = matrix(3,3);
	random_mtrx(mtrx1);
	random_mtrx(mtrx2);
	printf("Matrix 1 :\n");
	print(mtrx1);
	printf("Matrix 1 Shape : ");
	mtrx_shape(mtrx1);
	printf("Matrix 2 :\n");
	print(mtrx2);
	printf("Matrix 2 Shape : ");
	mtrx_shape(mtrx2);
	print(dot(*mtrx1,*mtrx2));
	double determinant = det(*mtrx2);
	printf("Determinant of Matrix 2 is %.2lf\n",determinant);
	printf("Adjoint of Matrix 2 is :\n");
	Matrix *adj = adjoint(mtrx2);
	print(adj);
	//print(mtrx_sum(*mtrx1,*mtrx2));
}
