%% Port of https://www.swi-prolog.org/pldoc/man?section=error
type_error(ValidType, Culprit) :- throw(type_error(ValidType, Culprit)).
domain_error(ValidDomain, Culprit) :- throw(domain_error(ValidDomain, Culprit)).
existence_error(ObjectType, Culprit) :- throw(existence_error(ObjectType, Culprit)).
existence_error(ObjectType, Culprit, Set) :- throw(existence_error(ObjectType, Culprit, Set)).
permission_error(Operation, PermissionType, Culprit) :- throw(permission_error(Operation, PermissionType, Culprit)).
instantiation_error(FormalSubTerm) :- throw(instantiation_error(FormalSubTerm)).
uninstantiation_error(Culprit) :- throw(uninstantiation_error(Culprit)).
representation_error(Flag) :- throw(representation_error(Flag)).
syntax_error(Culprit) :- throw(syntax_error(Culprit)).
resource_error(Resource) :- throw(resource_error(Resource)).

%% TODO SWI has is_of_type instead.
must_be(Type, Term) :- call(Type, Term), !.
must_be(Type, Term) :- type_error(Type, Term).
