use std::{iter, mem};
use std::collections::HashMap;

use crate::exceptions::{CompilationResult, LocatedException};
use crate::location::{Located, Location, Sequence};
use crate::OptimizationSettings;
use crate::parser::namespace_element::{NamespaceElement, NamespaceElementHolder};
use crate::reference::Reference;
use crate::type_checker::subroutines::{SubroutineSignature, TypeCheckedSubroutine, TypeCheckedSubroutineBody};
use crate::type_checker::types::{Type, Value};
use crate::utils::Visibility;

/// An entry holds a value, along with some metadata like its [`Location`] and [`Visibility`].
#[derive(Clone, Debug)]
struct Entry<T> {
    location: Location,
    visibility: Visibility,
    value: T,
}


/// A `T` scope contains multiple [`Entry<T>`] that can be registered and accessed via a key.
trait Scope<T> {
    const ELEMENT_DESCRIPTION: &'static str;

    /// Tries to register an entry.
    fn register_entry(&mut self, location: Location, identifier: String, entry: Entry<T>) -> CompilationResult<()>;

    /// Returns the optional entry corresponding to a key.
    fn get_entry(&self, identifier: &str) -> Option<&Entry<T>>;
}


/// A `Registry` maps keys (by default, strings) to [namespace entries](Entry) of a specific kind.
type Registry<T, K = String> = HashMap<K, Entry<T>>;


/// A convenience trait that automatically implements [`Scope<T>`] given a [`Registry<T>`].
trait RegistryBasedScope<T> {
    const ELEMENT_DESCRIPTION: &'static str;

    fn registry(&self) -> &Registry<T>;

    fn registry_mut(&mut self) -> &mut Registry<T>;
}

impl<T, S: RegistryBasedScope<T>> Scope<T> for S {
    const ELEMENT_DESCRIPTION: &'static str = Self::ELEMENT_DESCRIPTION;

    fn register_entry(&mut self, location: Location, identifier: String, entry: Entry<T>) -> CompilationResult<()> {
        match self.registry_mut().insert(identifier, entry) {
            None => Ok(()),
            Some(Entry { location: initial_location, .. }) => {
                Err(LocatedException::element_name_is_already_used(location, Self::ELEMENT_DESCRIPTION, initial_location))
            }
        }
    }

    fn get_entry(&self, identifier: &str) -> Option<&Entry<T>> {
        self.registry().get(identifier)
    }
}


/// The identifier of a subroutine.
#[derive(Copy, Clone, Debug)]
struct SubroutineID(usize);


/// A namespace defines a scope for types, subroutines, constants as well as other namespaces.
#[derive(Default, Debug)]
pub struct Namespace {
    namespaces: Registry<Namespace>,
    types: Registry<Type>,
    subroutines: Registry<SubroutineID>,
    constants: Registry<Value>,
}

impl Namespace {
    pub fn type_check_and_register_elements(context: &mut NamespaceContext, elements: Sequence<NamespaceElementHolder>, optimizations: &OptimizationSettings) -> CompilationResult<()> {
        elements.into_iter()
            .try_for_each(|Located { location, value: NamespaceElementHolder { visibility, identifier, element } }| {
                match element {
                    NamespaceElement::Constant(expression) => {
                        let value = Value::evaluate(context, expression)?;
                        context.register(location, identifier, visibility, value)
                    }
                    NamespaceElement::TypeAlias(type_descriptor) => {
                        let r#type = Type::resolve_descriptor(context, location.clone(), type_descriptor)?;
                        context.register(location, identifier, visibility, r#type)
                    }
                    NamespaceElement::Subroutine(arguments, return_type, body) => {
                        let signature = SubroutineSignature::from_untyped(context, arguments, return_type)?;
                        let type_checked_body = context.within_subroutine(|context| {
                            signature.register_variables(context)?;
                            TypeCheckedSubroutineBody::type_check(context, body, signature.return_type(), optimizations)
                        })?;
                        let subroutine = TypeCheckedSubroutine { signature, body: type_checked_body };
                        context.register_subroutine(location, identifier, visibility, subroutine)
                    }
                    NamespaceElement::Namespace(elements) => {
                        context.register_namespace(location, identifier, visibility, |context| {
                            Self::type_check_and_register_elements(context, elements, optimizations)
                        })
                    }
                }
            })
    }
}

impl RegistryBasedScope<Namespace> for Namespace {
    const ELEMENT_DESCRIPTION: &'static str = "namespace";

    fn registry(&self) -> &Registry<Namespace> {
        &self.namespaces
    }

    fn registry_mut(&mut self) -> &mut Registry<Namespace> {
        &mut self.namespaces
    }
}

impl RegistryBasedScope<Type> for Namespace {
    const ELEMENT_DESCRIPTION: &'static str = "type";

    fn registry(&self) -> &Registry<Type> {
        &self.types
    }

    fn registry_mut(&mut self) -> &mut Registry<Type> {
        &mut self.types
    }
}

impl RegistryBasedScope<SubroutineID> for Namespace {
    const ELEMENT_DESCRIPTION: &'static str = "subroutine";

    fn registry(&self) -> &Registry<SubroutineID> {
        &self.subroutines
    }

    fn registry_mut(&mut self) -> &mut Registry<SubroutineID> {
        &mut self.subroutines
    }
}

impl RegistryBasedScope<Value> for Namespace {
    const ELEMENT_DESCRIPTION: &'static str = "constant";

    fn registry(&self) -> &Registry<Value> {
        &self.constants
    }

    fn registry_mut(&mut self) -> &mut Registry<Value> {
        &mut self.constants
    }
}


/// The typing context of a namespace.
///
/// Consists of a stack of [`Namespace`].
#[derive(Default, Debug)]
pub struct NamespaceContext {
    subroutines: Vec<TypeCheckedSubroutine>,
    /// A stack of namespaces.
    stack: Vec<Namespace>,
    /// The current scope.
    active: Namespace,
}

impl NamespaceContext {
    /// Executes a function within the context of a subroutine in this context.
    ///
    /// # Why `&mut self`?
    ///
    /// This is a way to ensure `f` does not access the scope stack it was called on. `self` is not
    /// actually mutated in a noticeable way after a call to this method.
    pub fn within_subroutine<T>(&mut self, f: impl FnOnce(&mut SubroutineContext) -> T) -> T {
        let mut scope = SubroutineContext::from(mem::take(self));
        let result = f(&mut scope);
        mem::swap(self, &mut scope.namespace);
        result
    }

    /// Defines a new element on the currently active scope.
    fn register<T>(&mut self, location: Location, identifier: String, visibility: Visibility, element: T) -> CompilationResult<()>
    where
        Namespace: Scope<T>
    {
        let entry = Entry { location: location.clone(), visibility, value: element };
        self.active.register_entry(location, identifier, entry)
    }

    /// Defines a new namespace and registers it on the currently active scope.
    pub fn register_namespace(&mut self, location: Location, identifier: String, visibility: Visibility, builder: impl FnOnce(&mut Self) -> CompilationResult<()>) -> CompilationResult<()> {
        self.stack.push(mem::take(&mut self.active));
        builder(self)?;
        let namespace = mem::replace(&mut self.active, self.stack.pop().unwrap());
        self.register(location, identifier, visibility, namespace)
    }

    /// Defines a new subroutine and registers it on the currently active scope.
    pub fn register_subroutine(&mut self, location: Location, identifier: String, visibility: Visibility, subroutine: TypeCheckedSubroutine) -> CompilationResult<()> {
        let id = self.subroutines.len();
        self.subroutines.push(subroutine);
        self.register(location, identifier, visibility, SubroutineID(id))
    }

    /// Finds an element on the scope stack.
    fn find<T>(&self, location: Location, reference: &Reference) -> CompilationResult<&T>
    where
        Namespace: Scope<T>
    {
        let description = <Namespace as Scope<T>>::ELEMENT_DESCRIPTION;
        match &reference.namespace {
            // Foreign
            Some(Located { location: namespace_location, value: namespace }) => {
                let scope: &Namespace = self.find::<Namespace>(namespace_location.to_owned(), namespace)?;
                match scope.get_entry(&reference.identifier) {
                    Some(Entry { visibility: Visibility::Public, value, .. }) => {
                        Ok(value)
                    }
                    Some(_) => {
                        Err(LocatedException::inaccessible_element(location, description, reference))
                    }
                    None => {
                        Err(LocatedException::unknown_element(location, description, reference))
                    }
                }
            }
            // Local
            None => {
                self.stack.iter()
                    .chain(iter::once(&self.active))
                    .rev()
                    .find_map(|scope| {
                        scope.get_entry(&reference.identifier)
                    })
                    .map(|Entry { value, .. }| value)
                    .ok_or_else(|| LocatedException::unknown_element(location, description, reference))
            }
        }
    }

    /// Finds a type on the scope stack and returns a clone of it.
    pub fn find_type(&self, location: Location, reference: &Reference) -> CompilationResult<Type> {
        self.find(location, reference).cloned()
    }

    /// Finds the index corresponding to a subroutine on the scope stack.
    pub fn find_subroutine_index(&self, location: Location, reference: &Reference) -> CompilationResult<usize> {
        let id = self.find::<SubroutineID>(location, reference)?;
        Ok(id.0)
    }

    /// Finds the indices of all subroutines that directly belong to the specified namespace.
    #[cfg(test)]
    pub fn find_subroutine_indices(&self, location: Location, namespace_reference: &Reference) -> CompilationResult<HashMap<String, usize>> {
        let namespace = self.find::<Namespace>(location, namespace_reference)?;
        Ok(
            namespace.subroutines.iter()
                .map(|(name, entry)| (name.to_owned(), entry.value.0))
                .collect()
        )
    }

    /// Finds the signature corresponding to a subroutine on the scope stack.
    pub fn find_subroutine_signature(&self, location: Location, reference: &Reference) -> CompilationResult<&SubroutineSignature> {
        let index = self.find_subroutine_index(location, reference)?;
        Ok(&self.subroutines[index].signature)
    }

    /// Finds a constant on the scope stack.
    pub fn find_constant_value(&self, location: Location, reference: &Reference) -> CompilationResult<Value> {
        let value = self.find::<Value>(location, reference)?;
        Ok(value.clone())
    }

    /// Finds a constant on the scope stack and returns its value.
    pub fn find_constant_type(&self, location: Location, reference: &Reference) -> CompilationResult<Type> {
        let value = self.find::<Value>(location, reference)?;
        Ok(value.get_type())
    }

    /// Collects all subroutines in the active scope inside a vector.
    pub fn collect_subroutines(self) -> Vec<TypeCheckedSubroutine> {
        self.subroutines
    }
}


/// A scope for variables.
#[derive(Default, Debug)]
struct VariableScope {
    variables: HashMap<String, Located<Type>>,
}

impl VariableScope {
    pub fn register_variable(&mut self, location: Location, identifier: String, r#type: Type) -> CompilationResult<()> {
        match self.variables.insert(identifier, Located::new(location.clone(), r#type)) {
            None => Ok(()),
            Some(Located { location: original_location, .. }) => {
                Err(LocatedException::element_name_is_already_used(location, "variable", original_location))
            }
        }
    }

    pub fn get_variable_type(&self, identifier: &str) -> Option<Type> {
        self.variables.get(identifier)
            .map(|variable_type| variable_type.value.to_owned())
    }
}


/// The typing context of a subroutine.
///
/// Consists of a [`NamespaceContext`] and a stack of [`VariableScope`].
#[derive(Debug)]
pub struct SubroutineContext {
    namespace: NamespaceContext,
    stack: Vec<VariableScope>,
    active: VariableScope,
}

impl SubroutineContext {
    /// Executes a function within the context of a subscope of this context.
    ///
    /// # Why `&mut self`?
    ///
    /// This is a way to ensure `f` does not access the scope stack it was called on. `self` is not
    /// actually mutated in a noticeable way after a call to this method.
    pub fn with_subscope<T>(&mut self, f: impl FnOnce(&mut Self) -> CompilationResult<T>) -> CompilationResult<T> {
        self.stack.push(mem::take(&mut self.active));
        let result = f(self);
        self.active = self.stack.pop().unwrap();
        result
    }

    /// Returns a reference to the [`NamespaceContext`] associated with this [`SubroutineContext`].
    pub fn namespace_context(&self) -> &NamespaceContext {
        &self.namespace
    }

    /// Returns a mutable reference to the [`NamespaceContext`] associated with this
    /// [`SubroutineContext`].
    pub fn namespace_context_mut(&mut self) -> &mut NamespaceContext {
        &mut self.namespace
    }

    /// Register a variable on the active [`VariableScope`].
    pub fn register_variable(&mut self, location: Location, identifier: String, r#type: Type) -> CompilationResult<()> {
        self.active.register_variable(location, identifier, r#type)
    }

    /// Returns the type of the variable corresponding to an identifier.
    pub fn get_variable_type(&self, location: Location, identifier: &str) -> CompilationResult<Type> {
        self.stack.iter()
            .chain(iter::once(&self.active))
            .rev()
            .find_map(|scope| {
                scope.get_variable_type(identifier)
            })
            .ok_or_else(|| LocatedException::unknown_variable(location, identifier))
    }

    /// Returns the type of the variable or constant corresponding to a reference.
    pub fn get_value_type(&self, location: Location, reference: &Reference) -> CompilationResult<Type> {
        // FIXME: In case the value does not exist, the error message will only mention constants,
        //  not variables.
        if reference.namespace.is_none() {
            if let Ok(r#type) = self.get_variable_type(location.clone(), &reference.identifier) {
                return Ok(r#type);
            }
        }
        self.namespace.find_constant_type(location, reference)
    }
}

impl From<NamespaceContext> for SubroutineContext {
    /// Creates a new [`SubroutineContext`] within a [`NamespaceContext`].
    fn from(namespace: NamespaceContext) -> Self {
        Self {
            namespace,
            stack: Vec::new(),
            active: Default::default(),
        }
    }
}
