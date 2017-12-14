--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.10
-- Dumped by pg_dump version 9.5.10

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: author; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE author (
    id integer,
    type character varying(20),
    value text
);


ALTER TABLE author OWNER TO ben;

--
-- Name: author_id_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE author_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE author_id_seq OWNER TO ben;

--
-- Name: comment; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE comment (
    opinion integer NOT NULL,
    comment character varying
);


ALTER TABLE comment OWNER TO ben;

--
-- Name: comment_opinion_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE comment_opinion_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE comment_opinion_seq OWNER TO ben;

--
-- Name: comment_opinion_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE comment_opinion_seq OWNED BY comment.opinion;


--
-- Name: excerpt; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE excerpt (
    opinion integer,
    type character varying(20),
    value character varying
);


ALTER TABLE excerpt OWNER TO ben;

--
-- Name: looks; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE looks (
    firstlook timestamp without time zone NOT NULL,
    wf_user text NOT NULL,
    rootid integer NOT NULL,
    opinionid integer
);


ALTER TABLE looks OWNER TO ben;

--
-- Name: opinion; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE opinion (
    id integer NOT NULL,
    author integer NOT NULL,
    flag character varying(50) NOT NULL,
    votevalue integer NOT NULL,
    target text NOT NULL,
    datestamp timestamp without time zone NOT NULL,
    url text NOT NULL,
    rooturl integer NOT NULL
);


ALTER TABLE opinion OWNER TO ben;

--
-- Name: opinion_id_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE opinion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE opinion_id_seq OWNER TO ben;

--
-- Name: opinion_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE opinion_id_seq OWNED BY opinion.id;


--
-- Name: reference; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE reference (
    opinion integer NOT NULL,
    reference text
);


ALTER TABLE reference OWNER TO ben;

--
-- Name: reference2; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE reference2 (
    id integer,
    urlfrom text,
    urlto text
);


ALTER TABLE reference2 OWNER TO ben;

--
-- Name: reference_opinion_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE reference_opinion_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE reference_opinion_seq OWNER TO ben;

--
-- Name: reference_opinion_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE reference_opinion_seq OWNED BY reference.opinion;


--
-- Name: rooturl; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE rooturl (
    id integer NOT NULL,
    rooturl text,
    rooturl_real boolean NOT NULL
);


ALTER TABLE rooturl OWNER TO ben;

--
-- Name: rooturl_id_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE rooturl_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE rooturl_id_seq OWNER TO ben;

--
-- Name: rooturl_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE rooturl_id_seq OWNED BY rooturl.id;


--
-- Name: opinion; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY comment ALTER COLUMN opinion SET DEFAULT nextval('comment_opinion_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY opinion ALTER COLUMN id SET DEFAULT nextval('opinion_id_seq'::regclass);


--
-- Name: opinion; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY reference ALTER COLUMN opinion SET DEFAULT nextval('reference_opinion_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY rooturl ALTER COLUMN id SET DEFAULT nextval('rooturl_id_seq'::regclass);


--
-- Name: comment_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (opinion);


--
-- Name: opinion_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY opinion
    ADD CONSTRAINT opinion_pkey PRIMARY KEY (id);


--
-- Name: reference_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY reference
    ADD CONSTRAINT reference_pkey PRIMARY KEY (opinion);


--
-- Name: rooturl_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY rooturl
    ADD CONSTRAINT rooturl_pkey PRIMARY KEY (id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

