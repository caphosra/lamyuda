# lamyuda

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/caphosra/lamyuda/lint.yml?label=Lint)](https://github.com/caphosra/lamyuda/blob/master/.github/workflows/lint.yml)
[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/caphosra/lamyuda/deploy.yml?label=Deployment)](https://github.com/caphosra/lamyuda/blob/master/.github/workflows/deploy.yml)

A simple untyped $\lambda$-term manipulator written in Haskell.

## Features

Conducts $\beta$-reduction on a lambda term given until it turns into a normal form.

```
> (\x. x z x) \x. x (\x. x) x y
(λx. x z x) (λx. x (λx. x) x y)
→β (λx. x (λx. x) x y) z (λx. x (λx. x) x y)
→β z (λx. x) z y (λx. x (λx. x) x y)
Normal form.
```

### Predefined items

Natural numbers (ex. 0, 10) and boolean values (ex. true, false) will be interpreted as those in church encoding.

```
> 3
3
= λs. λz. s (s (s z))
Normal form.
> true
true
= λt. λf. t
Normal form.
```

### Defines a named function

You can define a named function and use it as the following.

```
> exp = \n. \m. m n
Defined: exp = λn. λm. m n
> exp 2 2
exp 2 2
= (λn. λm. m n) (λs. λz. s (s z)) (λs. λz. s (s z))
→β (λm. m (λs. λz. s (s z))) (λs. λz. s (s z))
→β (λs. λz. s (s z)) (λs. λz. s (s z))
→β λz. (λs. λz. s (s z)) ((λs. λz. s (s z)) z)
→β λz. λz'. (λs. λz. s (s z)) z ((λs. λz. s (s z)) z z')
→β λz. λz'. (λz'. z (z z')) ((λs. λz. s (s z)) z z')
→β λz. λz'. z (z ((λs. λz. s (s z)) z z'))
→β λz. λz'. z (z ((λz'. z (z z')) z'))
→β λz. λz'. z (z (z (z z')))
Normal form.
```

### Specifies a evaluation strategy

This is why I decided to create this project. You can specify a evaluation strategy by `#strategy`.

```
> #strategy cv
Strategy : Call by Value
> exp = \n. \m. m n
Defined: exp = λn. λm. m n
> exp 2 2
exp 2 2
= (λn. λm. m n) (λs. λz. s (s z)) (λs. λz. s (s z))
→β (λm. m (λs. λz. s (s z))) (λs. λz. s (s z))
→β (λs. λz. s (s z)) (λs. λz. s (s z))
→β λz. (λs. λz. s (s z)) ((λs. λz. s (s z)) z)
Normal form.
```

Supported strategies:
- Normal Order (Default, `#strategy no`)
- Call by Value (`#strategy cv`)
- Call by Name (`#strategy cn`)

### Enables $\eta$-reduction

You can choose $\beta\eta$-reduction instead of $\beta$-reduction. This feature is disabled by default.

```
> #enable eta
η-reduction feature enabled.
> \x. y x
λx. y x
→η y
Normal form.
> #disable eta
η-reduction feature disabled.
> \x. y x
λx. y x
Normal form.
```

### Evaluates other files

Use `#eval` to evaluate an external file.

```
> #eval "./sample/arith.lmd"
>> add = lambda m. lambda n. lambda s. lambda z. n s (m s z)
Defined: add = λm. λn. λs. λz. n s (m s z)
>> mul = lambda m. lambda n. lambda s. n (m s)
Defined: mul = λm. λn. λs. n (m s)
>> exp = lambda m. lambda n. n m
Defined: exp = λm. λn. n m
> add 1 1
add 1 1
= (λm. λn. λs. λz. n s (m s z)) (λs. λz. s z) (λs. λz. s z)
→β (λn. λs. λz. n s ((λs. λz. s z) s z)) (λs. λz. s z)
→β λs. λz. (λs. λz. s z) s ((λs. λz. s z) s z)
→β λs. λz. (λz. s z) ((λs. λz. s z) s z)
→β λs. λz. s ((λs. λz. s z) s z)
→β λs. λz. s ((λz. s z) z)
→β λs. λz. s (s z)
Normal form.
```

## Installation

### Build manually

Prepare a Haskell environment ([stack](https://github.com/commercialhaskell/stack.git) must be installed) and then execute the following command in it.
```bash
$ git clone https://github.com/caphosra/lamyuda.git
$ cd lamyuda
$ stack build --copy-bins
```

You may run `stack path` and find a local-bin section to get a path of the destination folder.

If you just want to run this without installation, try the following.
```bash
$ stack run
```

### Use Docker

lamyuda is available on the docker image. Please ensure that Docker is installed to your workspace.

```bash
$ docker run --rm -it ghcr.io/caphosra/lamyuda:latest
```

If you want to use a stable version of lamyuda, try changing a tag passing to Docker in order to specify the version of it.
