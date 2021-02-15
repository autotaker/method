# Revision history for method

## 0.3.0.0 -- TBA
* Add new Protocol DSL to write communication specification between multiple methods. [\#5](https://github.com/autotaker/method/issues/5)
  * Added new `Test.Method.Protocol` module.
  * Generalize `thenReturn`, `thenAction`, and `thenMethod` by using `Behave` type class.
  * Rename `throwNoStubShow` -> `throwNoStub` and `throwNoStub` -> `throwNoStubWithShow` to
    keep consistency with `lookupMock`/`lookupMockWithShow`.

## 0.2.0.0 -- 2021-01-10
* `throwNoStub` throws runtime exception instead of throwing exception via `MonadThrow`. [\#1](https://github.com/autotaker/method/issues/1)


## 0.1.0.0 -- 2021-01-09

* First stable version
