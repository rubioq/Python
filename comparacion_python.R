## Ecuación -x^2 - 2x + 3 = 0

(- (-2) + sqrt((-2)^2-4*-1*3))/(2*-1)

(- (-2) - sqrt((-2)^2-4*-1*3))/(2*-1)

## Función ecuación de segundo grado

ec.segundo.grado <- function (a,b,c) 
{
  previo <- b^2-(4*a*c)
  if (previo<0){
    stop ("Esta ecuación no tiene solución")
  }
  else {
    x1 <- (-b+sqrt(previo))/(2*a)
    x2 <- (-b-sqrt(previo))/(2*a)
    if (a*(x1^2)+b*x1+c==0 && a*(x2^2)+b*x2+c==0){
      print ("Ambas son correctas")
    }
    else if (a*(x1^2)+b*x1+c==0 && a*(x2^2)+b*x2+c!=0){
      print ("Solo x1 es correcta")
    }
    else {
      print("Solo x2 es correcta")
    }
  }
  return (c(x1,x2))
}

## Probamos la función

ec.segundo.grado(-1,-2,3)
