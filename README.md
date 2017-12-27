# Calculator
Calculator can solve expressions with mathematical operations posted below:
```
+ - * /
```
You can use parenthesis or nested parenthesis e.g.:
```
(1+2*(3*(5-4))-4)
(1+1)
```

Calculator only accept integers.

## Run calculator:
```
sbt run
```

The default port in play application is 9000.

## Solve expression

Endpoint to solve method is:
```
/evaluate
```

## Example using curl
```
curl -H "Content-Type: application/json" -X POST -d '{"expression":"(1-1)*2+3*(1-3+4)+10/2"}' http://localhost:9000/evaluate
```
Result:
```
{"result":11}
```

## Running on other port
If you want run calculator using custom port instead of default one you need to launch sbt
```
sbt
```
And next use run with custom port (e.g. 5555)
```
run 5555
```

### OR
Use command below:
```
sbt "run 5555"
```