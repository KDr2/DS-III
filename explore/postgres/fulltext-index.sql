
-- full text index

ALTER TABLE comment ADD COLUMN content_fti tsvector NULL;
CREATE INDEX fts_comment_tsvector_gin_idx ON comment USING GIN(content_fti);

CREATE OR REPLACE FUNCTION comment_fti_trigger()
RETURNS trigger AS
$BODY$
begin
   new.content_fti :=
       to_tsvector(coalesce(new.content,''));
       -- to_tsvector('english', new.content);
   return new;
end
$BODY$
LANGUAGE plpgsql VOLATILE COST 100;
-- ALTER FUNCTION messages_trigger() OWNER TO postgres;


CREATE TRIGGER t_comment_fti_trigger
BEFORE INSERT OR UPDATE
ON comment
FOR EACH ROW
EXECUTE PROCEDURE comment_fti_trigger();


select id from comment where content_fti@@to_tsquery('kw1,kw2');
select id from comment where content_fti@@plainto_tsquery('english', 'Discuss');
