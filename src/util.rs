pub struct RloxError {
    pub errors: Vec<String>
}

pub struct ErrorWithPartialResult<T> {
    pub error: RloxError,
    pub partial_result: T
}