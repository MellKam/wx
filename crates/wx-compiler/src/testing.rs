use std::collections::HashMap;

/// Serializes a HashMap with keys sorted for deterministic snapshot output.
pub fn serialize_sorted_map<K, V, S>(
    map: &HashMap<K, V>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    K: Ord + serde::Serialize,
    V: serde::Serialize,
    S: serde::Serializer,
{
    use serde::ser::SerializeMap;
    let mut pairs: Vec<_> = map.iter().collect();
    pairs.sort_by_key(|(k, _)| *k);
    let mut ser = serializer.serialize_map(Some(pairs.len()))?;
    for (k, v) in pairs {
        ser.serialize_entry(k, v)?;
    }
    ser.end()
}

/// Serializes a `HashMap<K, HashMap<K2, V>>` with both levels of keys sorted.
pub fn serialize_sorted_nested_map<K, K2, V, S>(
    map: &HashMap<K, HashMap<K2, V>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    K: Ord + serde::Serialize,
    K2: Ord + serde::Serialize,
    V: serde::Serialize,
    S: serde::Serializer,
{
    use serde::ser::SerializeMap;
    let mut outer: Vec<_> = map.iter().collect();
    outer.sort_by_key(|(k, _)| *k);
    let mut ser = serializer.serialize_map(Some(outer.len()))?;
    for (k, inner_map) in outer {
        let mut inner: Vec<_> = inner_map.iter().collect();
        inner.sort_by_key(|(k2, _)| *k2);
        ser.serialize_entry(k, &SortedMapRef(&inner))?;
    }
    ser.end()
}

pub struct SortedMapRef<'a, K, V>(&'a Vec<(&'a K, &'a V)>);

impl<K: serde::Serialize, V: serde::Serialize> serde::Serialize for SortedMapRef<'_, K, V> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut ser = serializer.serialize_map(Some(self.0.len()))?;
        for (k, v) in self.0 {
            ser.serialize_entry(k, v)?;
        }
        ser.end()
    }
}
