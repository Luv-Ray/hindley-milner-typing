use std::{
    collections::{HashMap, HashSet},
    iter,
    ops::{Index, IndexMut},
    sync::atomic::{AtomicU32, Ordering},
};

use crate::expr::Expr;

#[derive(Clone, PartialEq)]
pub enum ConstType {
    Unit,
    Int,
    String,
}

#[derive(Clone, PartialEq)]
pub enum TypeFunction {
    Arrow(Id, Id),
}

impl TypeFunction {
    pub fn parameters(&self) -> Vec<Id> {
        match self {
            Self::Arrow(ty1, ty2) => vec![*ty1, *ty2],
        }
    }

    pub fn matches(f1: &Self, f2: &Self) -> bool {
        match (f1, f2) {
            (Self::Arrow(_, _), Self::Arrow(_, _)) => true,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum MonoType<C> {
    Variable,
    Const(C),
    Func(TypeFunction),
}

#[derive(Clone)]
pub struct PolyType {
    bound: Id,
    ty: MonoId,
}

impl PolyType {
    // pub fn specialize(&self, ctx: &mut TyCtxt) -> Id {
    //     // let ty = self.ty
    // }
}

#[derive(Clone)]
pub enum Type<C> {
    Mono(MonoType<C>),
    Poly(PolyType),
}

impl<C> Type<C> {
    fn is_free(&self) -> bool {
        matches!(self, Type::Mono(MonoType::Variable))
    }
}

pub enum Error {
    Internal,
    UnMatch,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Id(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// id of [`MonoType`]
pub struct MonoId(u32);

impl MonoId {
    pub fn as_id(self) -> Id {
        Id(self.0)
    }
}

struct UnificationTable {
    generator: AtomicU32,
    table: Vec<Id>,
}

impl Index<Id> for Vec<Id> {
    type Output = Id;
    fn index(&self, index: Id) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl IndexMut<Id> for Vec<Id> {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

impl UnificationTable {
    pub fn new() -> Self {
        Self {
            generator: AtomicU32::new(0),
            table: Vec::new(),
        }
    }

    pub fn next_id(&self) -> Id {
        Id(self.generator.fetch_add(1, Ordering::Relaxed))
    }

    /// `id1` as the root of `id2`
    pub fn union(&mut self, id1: Id, id2: Id) {
        let root1 = self.find(id1);
        let root2 = self.find(id2);
        self.table[root2] = root1;
    }

    pub fn find(&mut self, id: Id) -> Id {
        if self.table[id] == id {
            id
        } else {
            self.table[id] = self.find(self.table[id]);
            self.table[id]
        }
    }
}

pub struct TyCtxt<C: PartialEq> {
    id_table: UnificationTable,
    map: HashMap<Id, Type<C>>,
}

impl<C: PartialEq> TyCtxt<C> {
    pub fn new() -> Self {
        Self {
            id_table: UnificationTable::new(),
            map: HashMap::new(),
        }
    }

    pub fn next_id(&self) -> Id {
        self.id_table.next_id()
    }

    pub fn register(&mut self, k: Id, v: Type<C>) {
        self.map.insert(k, v);
    }

    pub fn infer(&mut self, expr: Expr) -> Result<Type<C>> {
        match expr {
            Expr::Var(_) | Expr::Lit(_) => unreachable!(),
            Expr::Let(var, expr) => {
                todo!()
            }
            Expr::App(lambda, expr) => {
                let lambda = self.infer(*lambda)?;
                todo!()
            }
            Expr::Lam(_, _) => todo!(),
        }
    }

    pub fn free_variable(&mut self) -> HashSet<Id> {
        let mut to_remove = vec![];
        let mut free_variable = HashSet::new();
        for (id, ty) in &self.map {
            if id == &self.id_table.find(*id) {
                if ty.is_free() {
                    free_variable.insert(*id);
                }
            } else {
                to_remove.push(*id);
            }
        }
        for id in to_remove {
            self.map.remove(&id);
        }
        free_variable
    }

    pub fn free_variable_of_expr(&mut self, id: Id) -> HashSet<Id> {
        let (free, bound) = self.free_variable_of_expr_internal(id);
        free.difference(&bound).into_iter().cloned().collect()
    }

    fn free_variable_of_expr_internal(&mut self, id: Id) -> (HashSet<Id>, HashSet<Id>) {
        let (mut free, mut bound) = (HashSet::new(), HashSet::new());
        let Some(ty) = self.ty(id) else {
            return (free, bound);
        };
        match ty {
            Type::Mono(ty) => match ty {
                MonoType::Variable => {
                    free.insert(id);
                }
                MonoType::Const(_) => {}
                MonoType::Func(f) => match *f {
                    TypeFunction::Arrow(id1, id2) => {
                        let (new_free, new_bound) = self.free_variable_of_expr_internal(id1);
                        free.extend(new_free);
                        bound.extend(new_bound);

                        let (new_free, new_bound) = self.free_variable_of_expr_internal(id2);
                        free.extend(new_free);
                        bound.extend(new_bound);
                    }
                },
            },
            Type::Poly(ty) => todo!(),
        }
        (free, bound)
    }

    pub fn ty(&self, index: Id) -> Option<&Type<C>> {
        self.map.get(&index)
    }

    pub fn substitute(&mut self, index: Id, ty: Type<C>) -> Result<()> {
        *self.map.get_mut(&index).ok_or(Error::Internal)? = ty;
        Ok(())
    }

    pub fn unify(&mut self, t1: Id, t2: Id) -> Result<()> {
        let t1 = self.id_table.find(t1);
        let t2 = self.id_table.find(t2);

        let ty1 = self.ty(t1).ok_or(Error::Internal)?;
        let ty2 = self.ty(t2).ok_or(Error::Internal)?;

        match (ty1, ty2) {
            (Type::Mono(ty1), Type::Mono(ty2)) => match (ty1, ty2) {
                (MonoType::Variable, _) => {
                    self.id_table.union(t2, t1);
                }
                (_, MonoType::Variable) => {
                    self.id_table.union(t1, t2);
                }
                (MonoType::Const(c1), MonoType::Const(c2)) => {
                    if c1 == c2 {
                        self.id_table.union(t1, t2);
                    } else {
                        return Err(Error::UnMatch);
                    }
                }
                (MonoType::Func(f1), MonoType::Func(f2)) => {
                    if !TypeFunction::matches(f1, f2) {
                        return Err(Error::UnMatch);
                    }
                    let p1 = f1.parameters().into_iter();
                    let p2 = f2.parameters().into_iter();
                    for (ty1, ty2) in iter::zip(p1, p2) {
                        self.unify(ty1, ty2)?;
                    }
                    self.id_table.union(t1, t2);
                }
                _ => return Err(Error::UnMatch),
            },
            (Type::Poly(ty1), Type::Poly(ty2)) => {
                todo!()
            }
            (_, _) => return Err(Error::UnMatch),
        };

        Ok(())
    }
}
