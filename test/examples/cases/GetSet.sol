contract GetSet {
  value : Word ;

  function setValue (x) public {
    value = x ;
  }

  function getValue () public {
    return value ;
  }
}
