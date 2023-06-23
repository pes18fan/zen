package zen

RangeType :: enum {
    INCLUSIVE,
    EXCLUSIVE,
}

Range :: struct {
    start: int,
    end: int,
    type: RangeType,
}
