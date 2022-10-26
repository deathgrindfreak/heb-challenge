BEGIN;
  SET TRANSACTION READ WRITE;

  CREATE TABLE image (
    id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
    label TEXT NOT NULL
  );

  CREATE TABLE tag (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
  );

  CREATE TABLE image_tag (
    image_id UUID NOT NULL REFERENCES image(id),
    tag_id BIGSERIAL NOT NULL REFERENCES tag(id),
    PRIMARY KEY (image_id, tag_id)
  );
COMMIT;
