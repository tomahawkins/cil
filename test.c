#pragma JessieIntegerModel(modulo)
#pragma JessieFloatModel(math)
//@ ensures \result;
int f() { static int a = 1; return a++;}

