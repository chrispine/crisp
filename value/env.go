package value

type Env struct {
	bindings map[string]*Value
	parent   *Env
}

func NewEnv(parent *Env) *Env {
	return &Env{parent: parent}
}

var TopLevelEnv = NewEnv(nil)

func (e *Env) Get(name string) *Value {
	val, ok := e.bindings[name]
	if ok {
		return val
	}

	if e.parent != nil {
		return e.parent.Get(name)
	}

	panic("[env error] not defined: " + name)
	return nil
}

func (e *Env) Set(name string, val *Value) *Value {
	e.bindings[name] = val
	return val
}
