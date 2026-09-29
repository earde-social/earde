\restrict dbmate

-- Dumped from database version 16.15 (Ubuntu 16.15-0ubuntu0.24.04.1)
-- Dumped by pg_dump version 16.15 (Ubuntu 16.15-0ubuntu0.24.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: bump_community_realtime_generation(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.bump_community_realtime_generation(target integer) RETURNS void
    LANGUAGE sql
    AS $$
    INSERT INTO community_realtime_generations (community_id, generation, updated_at)
    SELECT id, 1, NOW() FROM communities WHERE id = target
    ON CONFLICT (community_id) DO UPDATE
       SET generation = community_realtime_generations.generation + 1,
           updated_at = NOW();
$$;


--
-- Name: community_realtime_access_row_removed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.community_realtime_access_row_removed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM bump_community_realtime_generation(OLD.community_id);
    RETURN NULL;
END;
$$;


--
-- Name: community_realtime_visibility_narrowed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.community_realtime_visibility_narrowed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM bump_community_realtime_generation(NEW.id);
    RETURN NULL;
END;
$$;


--
-- Name: user_realtime_access_narrowed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.user_realtime_access_narrowed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF OLD.is_admin AND NOT NEW.is_admin THEN
        PERFORM bump_community_realtime_generation(c.id)
           FROM communities c WHERE c.visibility = 'private';
    END IF;
    IF (NEW.is_banned AND NOT OLD.is_banned)
       OR (NEW.username LIKE '[deleted\_%' AND OLD.username NOT LIKE '[deleted\_%') THEN
        PERFORM bump_community_realtime_generation(x.community_id)
           FROM (SELECT community_id FROM community_members WHERE user_id = NEW.id
                 UNION
                 SELECT community_id FROM community_moderators WHERE user_id = NEW.id) x;
    END IF;
    RETURN NULL;
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: channels; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channels (
    id integer NOT NULL,
    community_id integer NOT NULL,
    slug text NOT NULL,
    name text NOT NULL,
    topic text,
    "position" integer DEFAULT 0 NOT NULL,
    is_archived boolean DEFAULT false NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    indexable boolean DEFAULT true NOT NULL
);


--
-- Name: channels_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.channels_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: channels_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.channels_id_seq OWNED BY public.channels.id;


--
-- Name: chat_messages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.chat_messages (
    id bigint NOT NULL,
    channel_id integer NOT NULL,
    user_id integer,
    content text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    edited_at timestamp without time zone,
    deleted_at timestamp without time zone,
    search_tsv tsvector
);


--
-- Name: chat_messages_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.chat_messages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: chat_messages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.chat_messages_id_seq OWNED BY public.chat_messages.id;


--
-- Name: comment_votes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.comment_votes (
    user_id integer NOT NULL,
    comment_id integer NOT NULL,
    direction integer NOT NULL
);


--
-- Name: comments; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.comments (
    id integer NOT NULL,
    content text NOT NULL,
    post_id integer NOT NULL,
    user_id integer NOT NULL,
    parent_id integer,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: comments_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.comments_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.comments_id_seq OWNED BY public.comments.id;


--
-- Name: communities; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.communities (
    id integer NOT NULL,
    slug text NOT NULL,
    name text NOT NULL,
    description text,
    rules text,
    avatar_url text,
    banner_url text,
    allow_downvotes boolean DEFAULT true NOT NULL,
    sections_enabled boolean DEFAULT true NOT NULL,
    visibility text DEFAULT 'public'::text NOT NULL,
    indexable boolean DEFAULT true NOT NULL,
    is_network_community boolean DEFAULT false NOT NULL,
    onboarding_state text DEFAULT 'published'::text NOT NULL,
    discoverable boolean DEFAULT true NOT NULL,
    CONSTRAINT communities_network_description_check CHECK (((NOT is_network_community) OR (description IS NULL) OR ((char_length(description) >= 1) AND (char_length(description) <= 2000) AND (description !~ '[\x01-\x08\x0b-\x1f\x7f]'::text) AND (description !~ '^[ \t\n]'::text) AND (description !~ '[ \t\n]$'::text)))),
    CONSTRAINT communities_network_lifecycle_check CHECK (((NOT is_network_community) OR (((onboarding_state = 'draft'::text) AND (visibility = 'private'::text) AND (NOT indexable) AND (NOT discoverable)) OR ((onboarding_state = 'published'::text) AND (visibility = 'public'::text) AND (indexable = discoverable))))),
    CONSTRAINT communities_network_name_check CHECK (((NOT is_network_community) OR ((char_length(name) >= 1) AND (char_length(name) <= 120) AND (name !~ '[\x01-\x1f\x7f]'::text) AND (name !~ '^ '::text) AND (name !~ ' $'::text)))),
    CONSTRAINT communities_network_slug_check CHECK (((NOT is_network_community) OR ((slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'::text) AND (char_length(slug) <= 80)))),
    CONSTRAINT communities_onboarding_state_check CHECK ((onboarding_state = ANY (ARRAY['draft'::text, 'published'::text]))),
    CONSTRAINT communities_visibility_check CHECK ((visibility = ANY (ARRAY['public'::text, 'private'::text])))
);


--
-- Name: communities_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.communities_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: communities_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.communities_id_seq OWNED BY public.communities.id;


--
-- Name: community_bans; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_bans (
    user_id integer NOT NULL,
    community_id integer NOT NULL,
    banned_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: community_connection_audit_events; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_connection_audit_events (
    id bigint NOT NULL,
    action text NOT NULL,
    actor_user_id integer,
    connection_id bigint NOT NULL,
    requester_community_id integer NOT NULL,
    recipient_community_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT community_connection_audit_events_action_check CHECK ((action = ANY (ARRAY['community_connection_requested'::text, 'community_connection_accepted'::text, 'community_connection_rejected'::text, 'community_connection_removed'::text])))
);


--
-- Name: community_connection_audit_events_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.community_connection_audit_events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: community_connection_audit_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.community_connection_audit_events_id_seq OWNED BY public.community_connection_audit_events.id;


--
-- Name: community_connections; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_connections (
    id bigint NOT NULL,
    requester_community_id integer NOT NULL,
    recipient_community_id integer NOT NULL,
    status text NOT NULL,
    requested_by_user_id integer,
    reviewed_by_user_id integer,
    removed_by_user_id integer,
    request_note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    reviewed_at timestamp with time zone,
    removed_at timestamp with time zone,
    CONSTRAINT community_connections_distinct_communities_check CHECK ((requester_community_id <> recipient_community_id)),
    CONSTRAINT community_connections_removed_after_created_check CHECK (((removed_at IS NULL) OR (removed_at >= created_at))),
    CONSTRAINT community_connections_removed_after_reviewed_check CHECK (((removed_at IS NULL) OR (reviewed_at IS NULL) OR (removed_at >= reviewed_at))),
    CONSTRAINT community_connections_request_note_check CHECK (((request_note IS NULL) OR (char_length(request_note) <= 2000))),
    CONSTRAINT community_connections_reviewed_after_created_check CHECK (((reviewed_at IS NULL) OR (reviewed_at >= created_at))),
    CONSTRAINT community_connections_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'accepted'::text, 'rejected'::text, 'removed'::text]))),
    CONSTRAINT community_connections_status_shape_check CHECK ((((status = 'pending'::text) AND (reviewed_at IS NULL) AND (removed_at IS NULL) AND (reviewed_by_user_id IS NULL) AND (removed_by_user_id IS NULL)) OR ((status = 'accepted'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NULL) AND (removed_by_user_id IS NULL)) OR ((status = 'rejected'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NULL) AND (removed_by_user_id IS NULL)) OR ((status = 'removed'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NOT NULL)))),
    CONSTRAINT community_connections_updated_after_created_check CHECK ((updated_at >= created_at))
);


--
-- Name: community_connections_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.community_connections_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: community_connections_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.community_connections_id_seq OWNED BY public.community_connections.id;


--
-- Name: community_members; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_members (
    user_id integer NOT NULL,
    community_id integer NOT NULL
);


--
-- Name: community_moderators; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_moderators (
    user_id integer NOT NULL,
    community_id integer NOT NULL,
    promoted_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    role character varying(50) DEFAULT 'mod'::character varying NOT NULL
);


--
-- Name: community_projects; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_projects (
    id bigint NOT NULL,
    project_id bigint NOT NULL,
    community_id integer NOT NULL,
    relation_type text DEFAULT 'home'::text NOT NULL,
    status text NOT NULL,
    requested_by_user_id integer,
    reviewed_by_user_id integer,
    request_note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    reviewed_at timestamp with time zone,
    removed_at timestamp with time zone,
    CONSTRAINT community_projects_relation_type_check CHECK ((relation_type = 'home'::text)),
    CONSTRAINT community_projects_removed_after_created_check CHECK (((removed_at IS NULL) OR (removed_at >= created_at))),
    CONSTRAINT community_projects_removed_after_reviewed_check CHECK (((removed_at IS NULL) OR (reviewed_at IS NULL) OR (removed_at >= reviewed_at))),
    CONSTRAINT community_projects_request_note_check CHECK (((request_note IS NULL) OR (char_length(request_note) <= 2000))),
    CONSTRAINT community_projects_reviewed_after_created_check CHECK (((reviewed_at IS NULL) OR (reviewed_at >= created_at))),
    CONSTRAINT community_projects_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'accepted'::text, 'rejected'::text, 'removed'::text]))),
    CONSTRAINT community_projects_status_shape_check CHECK ((((status = 'pending'::text) AND (reviewed_at IS NULL) AND (removed_at IS NULL) AND (reviewed_by_user_id IS NULL)) OR ((status = 'accepted'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NULL)) OR ((status = 'rejected'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NULL)) OR ((status = 'removed'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NOT NULL)))),
    CONSTRAINT community_projects_updated_after_created_check CHECK ((updated_at >= created_at))
);


--
-- Name: community_projects_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.community_projects_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: community_projects_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.community_projects_id_seq OWNED BY public.community_projects.id;


--
-- Name: community_realtime_generations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_realtime_generations (
    community_id integer NOT NULL,
    generation bigint DEFAULT 0 NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT community_realtime_generations_generation_check CHECK ((generation >= 0))
);


--
-- Name: community_sections; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_sections (
    id integer NOT NULL,
    community_id integer NOT NULL,
    name text NOT NULL,
    description text,
    "position" integer DEFAULT 0 NOT NULL,
    default_sort text DEFAULT 'new'::text NOT NULL,
    is_introduction_section boolean DEFAULT false NOT NULL,
    slug text DEFAULT ''::text NOT NULL,
    indexable boolean DEFAULT true NOT NULL
);


--
-- Name: community_sections_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.community_sections_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: community_sections_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.community_sections_id_seq OWNED BY public.community_sections.id;


--
-- Name: community_user_stats; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_user_stats (
    id integer NOT NULL,
    user_id integer NOT NULL,
    community_id integer NOT NULL,
    local_karma integer DEFAULT 0 NOT NULL,
    local_post_count integer DEFAULT 0 NOT NULL,
    local_comment_count integer DEFAULT 0 NOT NULL,
    first_active_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: community_user_stats_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.community_user_stats_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: community_user_stats_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.community_user_stats_id_seq OWNED BY public.community_user_stats.id;


--
-- Name: dream_session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dream_session (
    id text NOT NULL,
    label text NOT NULL,
    expires_at real NOT NULL,
    payload text NOT NULL
);


--
-- Name: github_installations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.github_installations (
    id bigint NOT NULL,
    github_installation_id bigint NOT NULL,
    github_account_id bigint NOT NULL,
    github_account_login text NOT NULL,
    github_account_type text NOT NULL,
    connected_by_user_id integer,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    revoked_at timestamp with time zone,
    CONSTRAINT github_installations_github_account_id_check CHECK ((github_account_id > 0)),
    CONSTRAINT github_installations_github_account_login_check CHECK ((btrim(github_account_login) <> ''::text)),
    CONSTRAINT github_installations_github_account_type_check CHECK ((github_account_type = ANY (ARRAY['user'::text, 'organization'::text]))),
    CONSTRAINT github_installations_github_installation_id_check CHECK ((github_installation_id > 0)),
    CONSTRAINT github_installations_revoked_at_check CHECK (((revoked_at IS NULL) OR (status = 'revoked'::text))),
    CONSTRAINT github_installations_status_check CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text, 'inaccessible'::text])))
);


--
-- Name: github_installations_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.github_installations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: github_installations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.github_installations_id_seq OWNED BY public.github_installations.id;


--
-- Name: github_onboarding_states; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.github_onboarding_states (
    id bigint NOT NULL,
    state_hash text NOT NULL,
    user_id integer NOT NULL,
    session_binding_hash text NOT NULL,
    flow text NOT NULL,
    pending_github_installation_id bigint,
    expires_at timestamp with time zone NOT NULL,
    consumed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT github_onboarding_states_consumed_after_created_check CHECK (((consumed_at IS NULL) OR (consumed_at >= created_at))),
    CONSTRAINT github_onboarding_states_expires_after_created_check CHECK ((expires_at > created_at)),
    CONSTRAINT github_onboarding_states_flow_check CHECK ((flow = 'project_onboarding'::text)),
    CONSTRAINT github_onboarding_states_pending_github_installation_id_check CHECK (((pending_github_installation_id IS NULL) OR (pending_github_installation_id > 0))),
    CONSTRAINT github_onboarding_states_session_binding_hash_check CHECK ((btrim(session_binding_hash) <> ''::text)),
    CONSTRAINT github_onboarding_states_state_hash_check CHECK ((btrim(state_hash) <> ''::text))
);


--
-- Name: github_onboarding_states_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.github_onboarding_states_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: github_onboarding_states_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.github_onboarding_states_id_seq OWNED BY public.github_onboarding_states.id;


--
-- Name: mod_actions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mod_actions (
    id integer NOT NULL,
    community_id integer NOT NULL,
    moderator_id integer NOT NULL,
    action_type character varying(50) NOT NULL,
    target_id integer,
    reason text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: mod_actions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.mod_actions_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: mod_actions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.mod_actions_id_seq OWNED BY public.mod_actions.id;


--
-- Name: notifications; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.notifications (
    id integer NOT NULL,
    user_id integer NOT NULL,
    post_id integer,
    message text,
    is_read boolean DEFAULT false NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    notif_type character varying(50) DEFAULT 'comment_reply'::character varying NOT NULL,
    actor_user_id integer,
    project_id bigint,
    community_id integer,
    relation_id bigint,
    connection_id bigint,
    shared_thread_placement_id bigint,
    CONSTRAINT notifications_notif_type_check CHECK (((notif_type)::text = ANY ((ARRAY['comment_reply'::character varying, 'mention'::character varying, 'mod_action'::character varying, 'project_home_requested'::character varying, 'project_home_accepted'::character varying, 'project_home_rejected'::character varying, 'project_home_removed'::character varying, 'community_connection_requested'::character varying, 'community_connection_accepted'::character varying, 'community_connection_rejected'::character varying, 'community_connection_removed'::character varying, 'shared_thread_requested'::character varying, 'shared_thread_accepted'::character varying, 'shared_thread_rejected'::character varying, 'shared_thread_removed'::character varying, 'shared_thread_withdrawn'::character varying])::text[]))),
    CONSTRAINT notifications_shape_check CHECK (((((notif_type)::text = ANY ((ARRAY['project_home_requested'::character varying, 'project_home_accepted'::character varying, 'project_home_rejected'::character varying, 'project_home_removed'::character varying])::text[])) AND (project_id IS NOT NULL) AND (community_id IS NOT NULL) AND (relation_id IS NOT NULL) AND (connection_id IS NULL) AND (shared_thread_placement_id IS NULL) AND (message IS NULL) AND (post_id IS NULL)) OR (((notif_type)::text = ANY ((ARRAY['community_connection_requested'::character varying, 'community_connection_accepted'::character varying, 'community_connection_rejected'::character varying, 'community_connection_removed'::character varying])::text[])) AND (community_id IS NOT NULL) AND (connection_id IS NOT NULL) AND (project_id IS NULL) AND (relation_id IS NULL) AND (shared_thread_placement_id IS NULL) AND (message IS NULL) AND (post_id IS NULL)) OR (((notif_type)::text = ANY ((ARRAY['shared_thread_requested'::character varying, 'shared_thread_accepted'::character varying, 'shared_thread_rejected'::character varying, 'shared_thread_removed'::character varying, 'shared_thread_withdrawn'::character varying])::text[])) AND (community_id IS NOT NULL) AND (shared_thread_placement_id IS NOT NULL) AND (project_id IS NULL) AND (relation_id IS NULL) AND (connection_id IS NULL) AND (message IS NULL) AND (post_id IS NULL)) OR (((notif_type)::text <> ALL ((ARRAY['project_home_requested'::character varying, 'project_home_accepted'::character varying, 'project_home_rejected'::character varying, 'project_home_removed'::character varying, 'community_connection_requested'::character varying, 'community_connection_accepted'::character varying, 'community_connection_rejected'::character varying, 'community_connection_removed'::character varying, 'shared_thread_requested'::character varying, 'shared_thread_accepted'::character varying, 'shared_thread_rejected'::character varying, 'shared_thread_removed'::character varying, 'shared_thread_withdrawn'::character varying])::text[])) AND (project_id IS NULL) AND (community_id IS NULL) AND (relation_id IS NULL) AND (connection_id IS NULL) AND (shared_thread_placement_id IS NULL) AND (actor_user_id IS NULL) AND (message IS NOT NULL))))
);


--
-- Name: notifications_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.notifications_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: notifications_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.notifications_id_seq OWNED BY public.notifications.id;


--
-- Name: open_source_projects; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.open_source_projects (
    id bigint NOT NULL,
    source_onboarding_draft_id bigint,
    name text NOT NULL,
    slug text NOT NULL,
    description text,
    website_url text,
    kind text NOT NULL,
    forge text DEFAULT 'github'::text NOT NULL,
    forge_namespace_id bigint NOT NULL,
    forge_namespace_login text NOT NULL,
    forge_namespace_type text NOT NULL,
    verification_status text DEFAULT 'verified'::text NOT NULL,
    created_by_user_id integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT open_source_projects_description_check CHECK (((description IS NULL) OR (char_length(description) <= 2000))),
    CONSTRAINT open_source_projects_forge_check CHECK ((forge = 'github'::text)),
    CONSTRAINT open_source_projects_forge_namespace_id_check CHECK ((forge_namespace_id > 0)),
    CONSTRAINT open_source_projects_forge_namespace_login_check CHECK (((forge_namespace_login = btrim(forge_namespace_login)) AND (forge_namespace_login <> ''::text) AND (char_length(forge_namespace_login) <= 255))),
    CONSTRAINT open_source_projects_forge_namespace_type_check CHECK ((forge_namespace_type = ANY (ARRAY['user'::text, 'organization'::text]))),
    CONSTRAINT open_source_projects_kind_check CHECK ((kind = ANY (ARRAY['project'::text, 'organization'::text, 'ecosystem'::text, 'foundation'::text, 'working_group'::text, 'other'::text]))),
    CONSTRAINT open_source_projects_name_check CHECK (((name = btrim(name)) AND ((char_length(name) >= 1) AND (char_length(name) <= 120)))),
    CONSTRAINT open_source_projects_slug_check CHECK (((slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'::text) AND (char_length(slug) <= 80))),
    CONSTRAINT open_source_projects_updated_after_created_check CHECK ((updated_at >= created_at)),
    CONSTRAINT open_source_projects_verification_status_check CHECK ((verification_status = ANY (ARRAY['verified'::text, 'stale'::text, 'revoked'::text]))),
    CONSTRAINT open_source_projects_website_url_check CHECK (((website_url IS NULL) OR ((website_url = btrim(website_url)) AND (website_url <> ''::text) AND (char_length(website_url) <= 2048))))
);


--
-- Name: open_source_projects_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.open_source_projects_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: open_source_projects_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.open_source_projects_id_seq OWNED BY public.open_source_projects.id;


--
-- Name: page_views; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.page_views (
    id integer NOT NULL,
    path text NOT NULL,
    referer text,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    session_hash text DEFAULT ''::text NOT NULL
);


--
-- Name: page_views_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.page_views_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: page_views_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.page_views_id_seq OWNED BY public.page_views.id;


--
-- Name: password_resets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.password_resets (
    id integer NOT NULL,
    user_id integer NOT NULL,
    token text NOT NULL,
    expires_at timestamp with time zone NOT NULL
);


--
-- Name: password_resets_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.password_resets_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: password_resets_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.password_resets_id_seq OWNED BY public.password_resets.id;


--
-- Name: pending_signups; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_signups (
    id bigint NOT NULL,
    username text NOT NULL,
    email text NOT NULL,
    password_hash text NOT NULL,
    token_hash text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    consumed_at timestamp with time zone,
    ip_address text,
    user_agent text
);


--
-- Name: pending_signups_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pending_signups_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pending_signups_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.pending_signups_id_seq OWNED BY public.pending_signups.id;


--
-- Name: post_votes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.post_votes (
    user_id integer NOT NULL,
    post_id integer NOT NULL,
    direction integer NOT NULL
);


--
-- Name: posthog_group_cleanup_jobs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posthog_group_cleanup_jobs (
    id bigint NOT NULL,
    group_key text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_attempt_at timestamp with time zone,
    completed_at timestamp with time zone,
    CONSTRAINT posthog_group_cleanup_jobs_attempts_check CHECK ((attempts >= 0)),
    CONSTRAINT posthog_group_cleanup_jobs_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'completed'::text])))
);


--
-- Name: posthog_group_cleanup_jobs_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.posthog_group_cleanup_jobs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: posthog_group_cleanup_jobs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.posthog_group_cleanup_jobs_id_seq OWNED BY public.posthog_group_cleanup_jobs.id;


--
-- Name: posthog_person_deletion_jobs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posthog_person_deletion_jobs (
    id bigint NOT NULL,
    distinct_id text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_attempt_at timestamp with time zone,
    completed_at timestamp with time zone,
    CONSTRAINT posthog_person_deletion_jobs_attempts_check CHECK ((attempts >= 0)),
    CONSTRAINT posthog_person_deletion_jobs_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'completed'::text])))
);


--
-- Name: posthog_person_deletion_jobs_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.posthog_person_deletion_jobs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: posthog_person_deletion_jobs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.posthog_person_deletion_jobs_id_seq OWNED BY public.posthog_person_deletion_jobs.id;


--
-- Name: posts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posts (
    id integer NOT NULL,
    title text NOT NULL,
    url text,
    content text,
    community_id integer NOT NULL,
    user_id integer NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    image_url text,
    section_id integer,
    last_activity_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    promoted_from_channel_id integer
);


--
-- Name: posts_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.posts_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: posts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.posts_id_seq OWNED BY public.posts.id;


--
-- Name: project_home_audit_events; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_home_audit_events (
    id bigint NOT NULL,
    action text NOT NULL,
    actor_user_id integer,
    project_id bigint NOT NULL,
    community_id integer NOT NULL,
    relation_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT project_home_audit_events_action_check CHECK ((action = ANY (ARRAY['project_home_requested'::text, 'project_home_accepted'::text, 'project_home_rejected'::text, 'project_home_removed'::text, 'dedicated_home_provisioned'::text, 'network_community_published'::text])))
);


--
-- Name: project_home_audit_events_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.project_home_audit_events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: project_home_audit_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.project_home_audit_events_id_seq OWNED BY public.project_home_audit_events.id;


--
-- Name: project_onboarding_draft_repositories; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_onboarding_draft_repositories (
    id bigint NOT NULL,
    draft_id bigint NOT NULL,
    "position" integer NOT NULL,
    github_repository_id bigint NOT NULL,
    github_owner_id bigint NOT NULL,
    owner_login text NOT NULL,
    name text NOT NULL,
    full_name text NOT NULL,
    html_url text NOT NULL,
    description text,
    default_branch text NOT NULL,
    is_archived boolean NOT NULL,
    is_selected boolean DEFAULT false NOT NULL,
    is_primary boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT project_onboarding_draft_repos_github_repository_id_check CHECK ((github_repository_id > 0)),
    CONSTRAINT project_onboarding_draft_repositories_default_branch_check CHECK ((btrim(default_branch) <> ''::text)),
    CONSTRAINT project_onboarding_draft_repositories_full_name_check CHECK ((btrim(full_name) <> ''::text)),
    CONSTRAINT project_onboarding_draft_repositories_github_owner_id_check CHECK ((github_owner_id > 0)),
    CONSTRAINT project_onboarding_draft_repositories_html_url_check CHECK ((btrim(html_url) <> ''::text)),
    CONSTRAINT project_onboarding_draft_repositories_name_check CHECK ((btrim(name) <> ''::text)),
    CONSTRAINT project_onboarding_draft_repositories_owner_login_check CHECK ((btrim(owner_login) <> ''::text)),
    CONSTRAINT project_onboarding_draft_repositories_position_check CHECK (("position" > 0)),
    CONSTRAINT project_onboarding_draft_repositories_primary_selected_check CHECK ((is_selected OR (NOT is_primary)))
);


--
-- Name: project_onboarding_draft_repositories_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.project_onboarding_draft_repositories_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: project_onboarding_draft_repositories_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.project_onboarding_draft_repositories_id_seq OWNED BY public.project_onboarding_draft_repositories.id;


--
-- Name: project_onboarding_drafts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_onboarding_drafts (
    id bigint NOT NULL,
    user_id integer NOT NULL,
    github_installation_record_id bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    completed_at timestamp with time zone,
    cancelled_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT project_onboarding_drafts_expires_after_created_check CHECK ((expires_at > created_at)),
    CONSTRAINT project_onboarding_drafts_lifecycle_check CHECK ((((status = 'active'::text) AND (completed_at IS NULL) AND (cancelled_at IS NULL)) OR ((status = 'completed'::text) AND (completed_at IS NOT NULL) AND (cancelled_at IS NULL)) OR ((status = 'cancelled'::text) AND (completed_at IS NULL) AND (cancelled_at IS NOT NULL)))),
    CONSTRAINT project_onboarding_drafts_status_check CHECK ((status = ANY (ARRAY['active'::text, 'completed'::text, 'cancelled'::text]))),
    CONSTRAINT project_onboarding_drafts_updated_after_created_check CHECK ((updated_at >= created_at))
);


--
-- Name: project_onboarding_drafts_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.project_onboarding_drafts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: project_onboarding_drafts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.project_onboarding_drafts_id_seq OWNED BY public.project_onboarding_drafts.id;


--
-- Name: project_repositories; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_repositories (
    id bigint NOT NULL,
    project_id bigint NOT NULL,
    "position" integer NOT NULL,
    github_repository_id bigint NOT NULL,
    full_name text NOT NULL,
    html_url text NOT NULL,
    description text,
    default_branch text NOT NULL,
    is_primary boolean DEFAULT false NOT NULL,
    is_archived boolean NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT project_repositories_default_branch_check CHECK ((btrim(default_branch) <> ''::text)),
    CONSTRAINT project_repositories_full_name_check CHECK ((btrim(full_name) <> ''::text)),
    CONSTRAINT project_repositories_github_repository_id_check CHECK ((github_repository_id > 0)),
    CONSTRAINT project_repositories_html_url_check CHECK ((btrim(html_url) <> ''::text)),
    CONSTRAINT project_repositories_position_check CHECK (("position" > 0)),
    CONSTRAINT project_repositories_updated_after_created_check CHECK ((updated_at >= created_at))
);


--
-- Name: project_repositories_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.project_repositories_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: project_repositories_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.project_repositories_id_seq OWNED BY public.project_repositories.id;


--
-- Name: project_stewards; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_stewards (
    project_id bigint NOT NULL,
    user_id integer NOT NULL,
    github_installation_record_id bigint NOT NULL,
    role text DEFAULT 'steward'::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT project_stewards_role_check CHECK ((role = 'steward'::text))
);


--
-- Name: rate_limits; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.rate_limits (
    ip_address text NOT NULL,
    endpoint text NOT NULL,
    attempts integer DEFAULT 1 NOT NULL,
    window_start real NOT NULL
);


--
-- Name: reports; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.reports (
    id integer NOT NULL,
    community_id integer NOT NULL,
    reporter_user_id integer NOT NULL,
    target_type text NOT NULL,
    target_id bigint NOT NULL,
    target_author_user_id integer,
    reason text NOT NULL,
    details text,
    status text DEFAULT 'open'::text NOT NULL,
    action_kind text,
    resolution_note text,
    resolved_by_user_id integer,
    resolved_at timestamp without time zone,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT reports_action_kind_check CHECK (((action_kind IS NULL) OR (action_kind = ANY (ARRAY['removed_content'::text, 'banned_author'::text, 'other'::text])))),
    CONSTRAINT reports_reason_check CHECK ((reason = ANY (ARRAY['spam'::text, 'abuse'::text, 'off_topic'::text, 'illegal'::text, 'other'::text]))),
    CONSTRAINT reports_status_check CHECK ((status = ANY (ARRAY['open'::text, 'dismissed'::text, 'action_taken'::text]))),
    CONSTRAINT reports_target_type_check CHECK ((target_type = ANY (ARRAY['post'::text, 'comment'::text, 'chat_message'::text])))
);


--
-- Name: reports_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.reports_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: reports_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.reports_id_seq OWNED BY public.reports.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying NOT NULL
);


--
-- Name: shared_thread_placement_audit_events; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.shared_thread_placement_audit_events (
    id bigint NOT NULL,
    action text NOT NULL,
    actor_user_id integer,
    placement_id bigint NOT NULL,
    post_id integer NOT NULL,
    origin_community_id integer NOT NULL,
    destination_community_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT shared_thread_placement_audit_events_action_check CHECK ((action = ANY (ARRAY['shared_thread_requested'::text, 'shared_thread_accepted'::text, 'shared_thread_rejected'::text, 'shared_thread_removed'::text, 'shared_thread_withdrawn'::text])))
);


--
-- Name: shared_thread_placement_audit_events_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.shared_thread_placement_audit_events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: shared_thread_placement_audit_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.shared_thread_placement_audit_events_id_seq OWNED BY public.shared_thread_placement_audit_events.id;


--
-- Name: shared_thread_placements; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.shared_thread_placements (
    id bigint NOT NULL,
    post_id integer NOT NULL,
    origin_community_id integer NOT NULL,
    destination_community_id integer NOT NULL,
    destination_section_id integer,
    status text NOT NULL,
    requested_by_user_id integer,
    reviewed_by_user_id integer,
    removed_by_user_id integer,
    withdrawn_by_user_id integer,
    request_note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    reviewed_at timestamp with time zone,
    removed_at timestamp with time zone,
    withdrawn_at timestamp with time zone,
    CONSTRAINT shared_thread_placements_distinct_communities_check CHECK ((destination_community_id <> origin_community_id)),
    CONSTRAINT shared_thread_placements_removed_after_created_check CHECK (((removed_at IS NULL) OR (removed_at >= created_at))),
    CONSTRAINT shared_thread_placements_removed_after_reviewed_check CHECK (((removed_at IS NULL) OR (reviewed_at IS NULL) OR (removed_at >= reviewed_at))),
    CONSTRAINT shared_thread_placements_request_note_check CHECK (((request_note IS NULL) OR (char_length(request_note) <= 2000))),
    CONSTRAINT shared_thread_placements_reviewed_after_created_check CHECK (((reviewed_at IS NULL) OR (reviewed_at >= created_at))),
    CONSTRAINT shared_thread_placements_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'accepted'::text, 'rejected'::text, 'removed'::text, 'withdrawn'::text]))),
    CONSTRAINT shared_thread_placements_status_shape_check CHECK ((((status = 'pending'::text) AND (reviewed_at IS NULL) AND (removed_at IS NULL) AND (withdrawn_at IS NULL) AND (reviewed_by_user_id IS NULL) AND (removed_by_user_id IS NULL) AND (withdrawn_by_user_id IS NULL) AND (destination_section_id IS NULL)) OR ((status = 'accepted'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NULL) AND (withdrawn_at IS NULL) AND (removed_by_user_id IS NULL) AND (withdrawn_by_user_id IS NULL)) OR ((status = 'rejected'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NULL) AND (withdrawn_at IS NULL) AND (removed_by_user_id IS NULL) AND (withdrawn_by_user_id IS NULL) AND (destination_section_id IS NULL)) OR ((status = 'removed'::text) AND (reviewed_at IS NOT NULL) AND (removed_at IS NOT NULL) AND (withdrawn_at IS NULL) AND (withdrawn_by_user_id IS NULL)) OR ((status = 'withdrawn'::text) AND (withdrawn_at IS NOT NULL) AND (reviewed_at IS NULL) AND (removed_at IS NULL) AND (reviewed_by_user_id IS NULL) AND (removed_by_user_id IS NULL) AND (destination_section_id IS NULL)))),
    CONSTRAINT shared_thread_placements_updated_after_created_check CHECK ((updated_at >= created_at)),
    CONSTRAINT shared_thread_placements_withdrawn_after_created_check CHECK (((withdrawn_at IS NULL) OR (withdrawn_at >= created_at)))
);


--
-- Name: shared_thread_placements_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.shared_thread_placements_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: shared_thread_placements_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.shared_thread_placements_id_seq OWNED BY public.shared_thread_placements.id;


--
-- Name: thread_source_messages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.thread_source_messages (
    post_id integer NOT NULL,
    message_id bigint NOT NULL,
    "position" integer DEFAULT 0 NOT NULL,
    is_seed boolean DEFAULT false NOT NULL
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id integer NOT NULL,
    username text NOT NULL,
    email text NOT NULL,
    password_hash text NOT NULL,
    verification_token text,
    is_email_verified boolean DEFAULT false NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    is_banned boolean DEFAULT false NOT NULL,
    bio text,
    avatar_url text,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_active_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: channels id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels ALTER COLUMN id SET DEFAULT nextval('public.channels_id_seq'::regclass);


--
-- Name: chat_messages id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages ALTER COLUMN id SET DEFAULT nextval('public.chat_messages_id_seq'::regclass);


--
-- Name: comments id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments ALTER COLUMN id SET DEFAULT nextval('public.comments_id_seq'::regclass);


--
-- Name: communities id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.communities ALTER COLUMN id SET DEFAULT nextval('public.communities_id_seq'::regclass);


--
-- Name: community_connection_audit_events id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connection_audit_events ALTER COLUMN id SET DEFAULT nextval('public.community_connection_audit_events_id_seq'::regclass);


--
-- Name: community_connections id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connections ALTER COLUMN id SET DEFAULT nextval('public.community_connections_id_seq'::regclass);


--
-- Name: community_projects id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_projects ALTER COLUMN id SET DEFAULT nextval('public.community_projects_id_seq'::regclass);


--
-- Name: community_sections id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections ALTER COLUMN id SET DEFAULT nextval('public.community_sections_id_seq'::regclass);


--
-- Name: community_user_stats id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats ALTER COLUMN id SET DEFAULT nextval('public.community_user_stats_id_seq'::regclass);


--
-- Name: github_installations id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations ALTER COLUMN id SET DEFAULT nextval('public.github_installations_id_seq'::regclass);


--
-- Name: github_onboarding_states id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states ALTER COLUMN id SET DEFAULT nextval('public.github_onboarding_states_id_seq'::regclass);


--
-- Name: mod_actions id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions ALTER COLUMN id SET DEFAULT nextval('public.mod_actions_id_seq'::regclass);


--
-- Name: notifications id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications ALTER COLUMN id SET DEFAULT nextval('public.notifications_id_seq'::regclass);


--
-- Name: open_source_projects id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.open_source_projects ALTER COLUMN id SET DEFAULT nextval('public.open_source_projects_id_seq'::regclass);


--
-- Name: page_views id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page_views ALTER COLUMN id SET DEFAULT nextval('public.page_views_id_seq'::regclass);


--
-- Name: password_resets id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets ALTER COLUMN id SET DEFAULT nextval('public.password_resets_id_seq'::regclass);


--
-- Name: pending_signups id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_signups ALTER COLUMN id SET DEFAULT nextval('public.pending_signups_id_seq'::regclass);


--
-- Name: posthog_group_cleanup_jobs id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_group_cleanup_jobs ALTER COLUMN id SET DEFAULT nextval('public.posthog_group_cleanup_jobs_id_seq'::regclass);


--
-- Name: posthog_person_deletion_jobs id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_person_deletion_jobs ALTER COLUMN id SET DEFAULT nextval('public.posthog_person_deletion_jobs_id_seq'::regclass);


--
-- Name: posts id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts ALTER COLUMN id SET DEFAULT nextval('public.posts_id_seq'::regclass);


--
-- Name: project_home_audit_events id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_home_audit_events ALTER COLUMN id SET DEFAULT nextval('public.project_home_audit_events_id_seq'::regclass);


--
-- Name: project_onboarding_draft_repositories id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_draft_repositories ALTER COLUMN id SET DEFAULT nextval('public.project_onboarding_draft_repositories_id_seq'::regclass);


--
-- Name: project_onboarding_drafts id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_drafts ALTER COLUMN id SET DEFAULT nextval('public.project_onboarding_drafts_id_seq'::regclass);


--
-- Name: project_repositories id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_repositories ALTER COLUMN id SET DEFAULT nextval('public.project_repositories_id_seq'::regclass);


--
-- Name: reports id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports ALTER COLUMN id SET DEFAULT nextval('public.reports_id_seq'::regclass);


--
-- Name: shared_thread_placement_audit_events id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placement_audit_events ALTER COLUMN id SET DEFAULT nextval('public.shared_thread_placement_audit_events_id_seq'::regclass);


--
-- Name: shared_thread_placements id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements ALTER COLUMN id SET DEFAULT nextval('public.shared_thread_placements_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: channels channels_community_id_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_community_id_slug_key UNIQUE (community_id, slug);


--
-- Name: channels channels_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_pkey PRIMARY KEY (id);


--
-- Name: chat_messages chat_messages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_pkey PRIMARY KEY (id);


--
-- Name: comment_votes comment_votes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comment_votes
    ADD CONSTRAINT comment_votes_pkey PRIMARY KEY (user_id, comment_id);


--
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- Name: communities communities_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.communities
    ADD CONSTRAINT communities_pkey PRIMARY KEY (id);


--
-- Name: communities communities_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.communities
    ADD CONSTRAINT communities_slug_key UNIQUE (slug);


--
-- Name: community_bans community_bans_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_bans
    ADD CONSTRAINT community_bans_pkey PRIMARY KEY (user_id, community_id);


--
-- Name: community_connection_audit_events community_connection_audit_events_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connection_audit_events
    ADD CONSTRAINT community_connection_audit_events_pkey PRIMARY KEY (id);


--
-- Name: community_connections community_connections_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connections
    ADD CONSTRAINT community_connections_pkey PRIMARY KEY (id);


--
-- Name: community_members community_members_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_members
    ADD CONSTRAINT community_members_pkey PRIMARY KEY (user_id, community_id);


--
-- Name: community_moderators community_moderators_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_moderators
    ADD CONSTRAINT community_moderators_pkey PRIMARY KEY (user_id, community_id);


--
-- Name: community_projects community_projects_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_projects
    ADD CONSTRAINT community_projects_pkey PRIMARY KEY (id);


--
-- Name: community_realtime_generations community_realtime_generations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_realtime_generations
    ADD CONSTRAINT community_realtime_generations_pkey PRIMARY KEY (community_id);


--
-- Name: community_sections community_sections_community_id_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections
    ADD CONSTRAINT community_sections_community_id_slug_key UNIQUE (community_id, slug);


--
-- Name: community_sections community_sections_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections
    ADD CONSTRAINT community_sections_pkey PRIMARY KEY (id);


--
-- Name: community_user_stats community_user_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_pkey PRIMARY KEY (id);


--
-- Name: community_user_stats community_user_stats_user_id_community_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_user_id_community_id_key UNIQUE (user_id, community_id);


--
-- Name: dream_session dream_session_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dream_session
    ADD CONSTRAINT dream_session_pkey PRIMARY KEY (id);


--
-- Name: github_installations github_installations_github_installation_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations
    ADD CONSTRAINT github_installations_github_installation_id_key UNIQUE (github_installation_id);


--
-- Name: github_installations github_installations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations
    ADD CONSTRAINT github_installations_pkey PRIMARY KEY (id);


--
-- Name: github_onboarding_states github_onboarding_states_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states
    ADD CONSTRAINT github_onboarding_states_pkey PRIMARY KEY (id);


--
-- Name: github_onboarding_states github_onboarding_states_state_hash_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states
    ADD CONSTRAINT github_onboarding_states_state_hash_key UNIQUE (state_hash);


--
-- Name: mod_actions mod_actions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions
    ADD CONSTRAINT mod_actions_pkey PRIMARY KEY (id);


--
-- Name: notifications notifications_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_pkey PRIMARY KEY (id);


--
-- Name: open_source_projects open_source_projects_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.open_source_projects
    ADD CONSTRAINT open_source_projects_pkey PRIMARY KEY (id);


--
-- Name: open_source_projects open_source_projects_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.open_source_projects
    ADD CONSTRAINT open_source_projects_slug_key UNIQUE (slug);


--
-- Name: page_views page_views_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page_views
    ADD CONSTRAINT page_views_pkey PRIMARY KEY (id);


--
-- Name: password_resets password_resets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_pkey PRIMARY KEY (id);


--
-- Name: password_resets password_resets_token_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_token_key UNIQUE (token);


--
-- Name: pending_signups pending_signups_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_signups
    ADD CONSTRAINT pending_signups_pkey PRIMARY KEY (id);


--
-- Name: pending_signups pending_signups_token_hash_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_signups
    ADD CONSTRAINT pending_signups_token_hash_key UNIQUE (token_hash);


--
-- Name: post_votes post_votes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.post_votes
    ADD CONSTRAINT post_votes_pkey PRIMARY KEY (user_id, post_id);


--
-- Name: posthog_group_cleanup_jobs posthog_group_cleanup_jobs_group_key_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_group_cleanup_jobs
    ADD CONSTRAINT posthog_group_cleanup_jobs_group_key_key UNIQUE (group_key);


--
-- Name: posthog_group_cleanup_jobs posthog_group_cleanup_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_group_cleanup_jobs
    ADD CONSTRAINT posthog_group_cleanup_jobs_pkey PRIMARY KEY (id);


--
-- Name: posthog_person_deletion_jobs posthog_person_deletion_jobs_distinct_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_person_deletion_jobs
    ADD CONSTRAINT posthog_person_deletion_jobs_distinct_id_key UNIQUE (distinct_id);


--
-- Name: posthog_person_deletion_jobs posthog_person_deletion_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_person_deletion_jobs
    ADD CONSTRAINT posthog_person_deletion_jobs_pkey PRIMARY KEY (id);


--
-- Name: posts posts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);


--
-- Name: project_home_audit_events project_home_audit_events_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_home_audit_events
    ADD CONSTRAINT project_home_audit_events_pkey PRIMARY KEY (id);


--
-- Name: project_onboarding_draft_repositories project_onboarding_draft_repos_draft_full_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_draft_repositories
    ADD CONSTRAINT project_onboarding_draft_repos_draft_full_name_key UNIQUE (draft_id, full_name);


--
-- Name: project_onboarding_draft_repositories project_onboarding_draft_repos_draft_position_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_draft_repositories
    ADD CONSTRAINT project_onboarding_draft_repos_draft_position_key UNIQUE (draft_id, "position");


--
-- Name: project_onboarding_draft_repositories project_onboarding_draft_repos_draft_repository_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_draft_repositories
    ADD CONSTRAINT project_onboarding_draft_repos_draft_repository_id_key UNIQUE (draft_id, github_repository_id);


--
-- Name: project_onboarding_draft_repositories project_onboarding_draft_repositories_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_draft_repositories
    ADD CONSTRAINT project_onboarding_draft_repositories_pkey PRIMARY KEY (id);


--
-- Name: project_onboarding_drafts project_onboarding_drafts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_drafts
    ADD CONSTRAINT project_onboarding_drafts_pkey PRIMARY KEY (id);


--
-- Name: project_repositories project_repositories_github_repository_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_repositories
    ADD CONSTRAINT project_repositories_github_repository_id_key UNIQUE (github_repository_id);


--
-- Name: project_repositories project_repositories_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_repositories
    ADD CONSTRAINT project_repositories_pkey PRIMARY KEY (id);


--
-- Name: project_repositories project_repositories_project_full_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_repositories
    ADD CONSTRAINT project_repositories_project_full_name_key UNIQUE (project_id, full_name);


--
-- Name: project_repositories project_repositories_project_position_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_repositories
    ADD CONSTRAINT project_repositories_project_position_key UNIQUE (project_id, "position");


--
-- Name: project_stewards project_stewards_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_stewards
    ADD CONSTRAINT project_stewards_pkey PRIMARY KEY (project_id, user_id);


--
-- Name: rate_limits rate_limits_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rate_limits
    ADD CONSTRAINT rate_limits_pkey PRIMARY KEY (ip_address, endpoint);


--
-- Name: reports reports_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: shared_thread_placement_audit_events shared_thread_placement_audit_events_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placement_audit_events
    ADD CONSTRAINT shared_thread_placement_audit_events_pkey PRIMARY KEY (id);


--
-- Name: shared_thread_placements shared_thread_placements_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_pkey PRIMARY KEY (id);


--
-- Name: thread_source_messages thread_source_messages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.thread_source_messages
    ADD CONSTRAINT thread_source_messages_pkey PRIMARY KEY (post_id, message_id);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: community_connections_one_active_pair_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX community_connections_one_active_pair_idx ON public.community_connections USING btree (LEAST(requester_community_id, recipient_community_id), GREATEST(requester_community_id, recipient_community_id)) WHERE (status = ANY (ARRAY['pending'::text, 'accepted'::text]));


--
-- Name: community_projects_one_active_home_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX community_projects_one_active_home_idx ON public.community_projects USING btree (project_id) WHERE ((relation_type = 'home'::text) AND (status = ANY (ARRAY['pending'::text, 'accepted'::text])));


--
-- Name: idx_chat_messages_channel_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_chat_messages_channel_id ON public.chat_messages USING btree (channel_id, id);


--
-- Name: idx_chat_messages_search_tsv; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_chat_messages_search_tsv ON public.chat_messages USING gin (search_tsv);


--
-- Name: idx_community_connection_audit_events_actor; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_connection_audit_events_actor ON public.community_connection_audit_events USING btree (actor_user_id, created_at DESC);


--
-- Name: idx_community_connection_audit_events_connection; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_connection_audit_events_connection ON public.community_connection_audit_events USING btree (connection_id, created_at DESC);


--
-- Name: idx_community_connection_audit_events_recipient; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_connection_audit_events_recipient ON public.community_connection_audit_events USING btree (recipient_community_id, created_at DESC);


--
-- Name: idx_community_connection_audit_events_requester; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_connection_audit_events_requester ON public.community_connection_audit_events USING btree (requester_community_id, created_at DESC);


--
-- Name: idx_community_connections_pair_history; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_connections_pair_history ON public.community_connections USING btree (LEAST(requester_community_id, recipient_community_id), GREATEST(requester_community_id, recipient_community_id), created_at DESC);


--
-- Name: idx_community_connections_recipient_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_connections_recipient_status ON public.community_connections USING btree (recipient_community_id, status, created_at);


--
-- Name: idx_community_connections_requester_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_connections_requester_status ON public.community_connections USING btree (requester_community_id, status, created_at);


--
-- Name: idx_community_projects_community_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_projects_community_status ON public.community_projects USING btree (community_id, status, created_at);


--
-- Name: idx_community_projects_project_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_projects_project_id ON public.community_projects USING btree (project_id);


--
-- Name: idx_community_user_stats_community_karma; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_user_stats_community_karma ON public.community_user_stats USING btree (community_id, local_karma);


--
-- Name: idx_community_user_stats_community_post_count; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_user_stats_community_post_count ON public.community_user_stats USING btree (community_id, local_post_count);


--
-- Name: idx_github_onboarding_states_expires_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_github_onboarding_states_expires_at ON public.github_onboarding_states USING btree (expires_at);


--
-- Name: idx_github_onboarding_states_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_github_onboarding_states_user_id ON public.github_onboarding_states USING btree (user_id);


--
-- Name: idx_notifications_actor; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_notifications_actor ON public.notifications USING btree (actor_user_id) WHERE (actor_user_id IS NOT NULL);


--
-- Name: idx_notifications_community; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_notifications_community ON public.notifications USING btree (community_id) WHERE (community_id IS NOT NULL);


--
-- Name: idx_notifications_connection; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_notifications_connection ON public.notifications USING btree (connection_id) WHERE (connection_id IS NOT NULL);


--
-- Name: idx_notifications_project; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_notifications_project ON public.notifications USING btree (project_id) WHERE (project_id IS NOT NULL);


--
-- Name: idx_notifications_relation; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_notifications_relation ON public.notifications USING btree (relation_id) WHERE (relation_id IS NOT NULL);


--
-- Name: idx_notifications_shared_thread_placement; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_notifications_shared_thread_placement ON public.notifications USING btree (shared_thread_placement_id) WHERE (shared_thread_placement_id IS NOT NULL);


--
-- Name: idx_open_source_projects_namespace; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_open_source_projects_namespace ON public.open_source_projects USING btree (forge_namespace_id, forge_namespace_type);


--
-- Name: idx_open_source_projects_verification_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_open_source_projects_verification_status ON public.open_source_projects USING btree (verification_status);


--
-- Name: idx_pending_signups_email_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_pending_signups_email_active ON public.pending_signups USING btree (lower(email)) WHERE (consumed_at IS NULL);


--
-- Name: idx_pending_signups_expires_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pending_signups_expires_at ON public.pending_signups USING btree (expires_at);


--
-- Name: idx_pending_signups_username_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_pending_signups_username_active ON public.pending_signups USING btree (lower(username)) WHERE (consumed_at IS NULL);


--
-- Name: idx_posthog_deletion_jobs_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_posthog_deletion_jobs_pending ON public.posthog_person_deletion_jobs USING btree (created_at) WHERE (status = 'pending'::text);


--
-- Name: idx_posthog_group_cleanup_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_posthog_group_cleanup_pending ON public.posthog_group_cleanup_jobs USING btree (created_at) WHERE (status = 'pending'::text);


--
-- Name: idx_project_home_audit_events_actor; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_home_audit_events_actor ON public.project_home_audit_events USING btree (actor_user_id, created_at DESC);


--
-- Name: idx_project_home_audit_events_community; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_home_audit_events_community ON public.project_home_audit_events USING btree (community_id, created_at DESC);


--
-- Name: idx_project_home_audit_events_project; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_home_audit_events_project ON public.project_home_audit_events USING btree (project_id, created_at DESC);


--
-- Name: idx_project_home_audit_events_relation; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_home_audit_events_relation ON public.project_home_audit_events USING btree (relation_id, created_at DESC);


--
-- Name: idx_project_onboarding_drafts_installation_record_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_onboarding_drafts_installation_record_id ON public.project_onboarding_drafts USING btree (github_installation_record_id);


--
-- Name: idx_project_onboarding_drafts_user_id_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_onboarding_drafts_user_id_active ON public.project_onboarding_drafts USING btree (user_id) WHERE (status = 'active'::text);


--
-- Name: idx_project_repositories_project_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_repositories_project_id ON public.project_repositories USING btree (project_id);


--
-- Name: idx_project_stewards_installation_record_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_stewards_installation_record_id ON public.project_stewards USING btree (github_installation_record_id);


--
-- Name: idx_project_stewards_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_project_stewards_user_id ON public.project_stewards USING btree (user_id);


--
-- Name: idx_reports_community_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_reports_community_status ON public.reports USING btree (community_id, status, created_at DESC);


--
-- Name: idx_shared_thread_placement_audit_events_actor; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placement_audit_events_actor ON public.shared_thread_placement_audit_events USING btree (actor_user_id, created_at DESC);


--
-- Name: idx_shared_thread_placement_audit_events_destination; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placement_audit_events_destination ON public.shared_thread_placement_audit_events USING btree (destination_community_id, created_at DESC);


--
-- Name: idx_shared_thread_placement_audit_events_origin; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placement_audit_events_origin ON public.shared_thread_placement_audit_events USING btree (origin_community_id, created_at DESC);


--
-- Name: idx_shared_thread_placement_audit_events_placement; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placement_audit_events_placement ON public.shared_thread_placement_audit_events USING btree (placement_id, created_at DESC);


--
-- Name: idx_shared_thread_placement_audit_events_post; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placement_audit_events_post ON public.shared_thread_placement_audit_events USING btree (post_id, created_at DESC);


--
-- Name: idx_shared_thread_placements_destination_section; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placements_destination_section ON public.shared_thread_placements USING btree (destination_section_id) WHERE (destination_section_id IS NOT NULL);


--
-- Name: idx_shared_thread_placements_destination_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placements_destination_status ON public.shared_thread_placements USING btree (destination_community_id, status, created_at);


--
-- Name: idx_shared_thread_placements_origin_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placements_origin_status ON public.shared_thread_placements USING btree (origin_community_id, status, created_at);


--
-- Name: idx_shared_thread_placements_post; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_shared_thread_placements_post ON public.shared_thread_placements USING btree (post_id, status);


--
-- Name: shared_thread_placements_one_active_destination_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX shared_thread_placements_one_active_destination_idx ON public.shared_thread_placements USING btree (post_id, destination_community_id) WHERE (status = ANY (ARRAY['pending'::text, 'accepted'::text]));


--
-- Name: uniq_open_source_projects_source_draft; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_open_source_projects_source_draft ON public.open_source_projects USING btree (source_onboarding_draft_id) WHERE (source_onboarding_draft_id IS NOT NULL);


--
-- Name: uniq_project_onboarding_draft_repos_primary_per_draft; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_project_onboarding_draft_repos_primary_per_draft ON public.project_onboarding_draft_repositories USING btree (draft_id) WHERE is_primary;


--
-- Name: uniq_project_onboarding_drafts_active_per_user_installation; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_project_onboarding_drafts_active_per_user_installation ON public.project_onboarding_drafts USING btree (user_id, github_installation_record_id) WHERE (status = 'active'::text);


--
-- Name: uniq_project_repositories_primary_per_project; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_project_repositories_primary_per_project ON public.project_repositories USING btree (project_id) WHERE is_primary;


--
-- Name: uniq_reports_open_per_reporter_target; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_reports_open_per_reporter_target ON public.reports USING btree (reporter_user_id, target_type, target_id) WHERE (status = 'open'::text);


--
-- Name: uniq_thread_source_seed_message; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_thread_source_seed_message ON public.thread_source_messages USING btree (message_id) WHERE is_seed;


--
-- Name: uq_notifications_recipient_kind_connection; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_notifications_recipient_kind_connection ON public.notifications USING btree (user_id, notif_type, connection_id) WHERE (connection_id IS NOT NULL);


--
-- Name: uq_notifications_recipient_kind_placement; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_notifications_recipient_kind_placement ON public.notifications USING btree (user_id, notif_type, shared_thread_placement_id) WHERE (shared_thread_placement_id IS NOT NULL);


--
-- Name: uq_notifications_recipient_kind_relation; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_notifications_recipient_kind_relation ON public.notifications USING btree (user_id, notif_type, relation_id) WHERE (relation_id IS NOT NULL);


--
-- Name: communities communities_realtime_access; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER communities_realtime_access AFTER UPDATE OF visibility ON public.communities FOR EACH ROW WHEN (((old.visibility IS DISTINCT FROM new.visibility) AND (new.visibility = 'private'::text))) EXECUTE FUNCTION public.community_realtime_visibility_narrowed();


--
-- Name: community_members community_members_realtime_access; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER community_members_realtime_access AFTER DELETE ON public.community_members FOR EACH ROW EXECUTE FUNCTION public.community_realtime_access_row_removed();


--
-- Name: community_moderators community_moderators_realtime_access; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER community_moderators_realtime_access AFTER DELETE ON public.community_moderators FOR EACH ROW EXECUTE FUNCTION public.community_realtime_access_row_removed();


--
-- Name: users users_realtime_access; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER users_realtime_access AFTER UPDATE OF is_admin, is_banned, username ON public.users FOR EACH ROW EXECUTE FUNCTION public.user_realtime_access_narrowed();


--
-- Name: channels channels_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: chat_messages chat_messages_channel_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_channel_id_fkey FOREIGN KEY (channel_id) REFERENCES public.channels(id) ON DELETE CASCADE;


--
-- Name: chat_messages chat_messages_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: comment_votes comment_votes_comment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comment_votes
    ADD CONSTRAINT comment_votes_comment_id_fkey FOREIGN KEY (comment_id) REFERENCES public.comments(id) ON DELETE CASCADE;


--
-- Name: comment_votes comment_votes_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comment_votes
    ADD CONSTRAINT comment_votes_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: comments comments_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES public.comments(id) ON DELETE CASCADE;


--
-- Name: comments comments_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- Name: comments comments_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: community_bans community_bans_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_bans
    ADD CONSTRAINT community_bans_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_bans community_bans_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_bans
    ADD CONSTRAINT community_bans_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: community_connection_audit_events community_connection_audit_events_actor_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connection_audit_events
    ADD CONSTRAINT community_connection_audit_events_actor_user_id_fkey FOREIGN KEY (actor_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: community_connection_audit_events community_connection_audit_events_connection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connection_audit_events
    ADD CONSTRAINT community_connection_audit_events_connection_id_fkey FOREIGN KEY (connection_id) REFERENCES public.community_connections(id);


--
-- Name: community_connection_audit_events community_connection_audit_events_recipient_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connection_audit_events
    ADD CONSTRAINT community_connection_audit_events_recipient_community_id_fkey FOREIGN KEY (recipient_community_id) REFERENCES public.communities(id);


--
-- Name: community_connection_audit_events community_connection_audit_events_requester_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connection_audit_events
    ADD CONSTRAINT community_connection_audit_events_requester_community_id_fkey FOREIGN KEY (requester_community_id) REFERENCES public.communities(id);


--
-- Name: community_connections community_connections_recipient_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connections
    ADD CONSTRAINT community_connections_recipient_community_id_fkey FOREIGN KEY (recipient_community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_connections community_connections_removed_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connections
    ADD CONSTRAINT community_connections_removed_by_user_id_fkey FOREIGN KEY (removed_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: community_connections community_connections_requested_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connections
    ADD CONSTRAINT community_connections_requested_by_user_id_fkey FOREIGN KEY (requested_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: community_connections community_connections_requester_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connections
    ADD CONSTRAINT community_connections_requester_community_id_fkey FOREIGN KEY (requester_community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_connections community_connections_reviewed_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_connections
    ADD CONSTRAINT community_connections_reviewed_by_user_id_fkey FOREIGN KEY (reviewed_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: community_members community_members_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_members
    ADD CONSTRAINT community_members_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_members community_members_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_members
    ADD CONSTRAINT community_members_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: community_moderators community_moderators_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_moderators
    ADD CONSTRAINT community_moderators_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_moderators community_moderators_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_moderators
    ADD CONSTRAINT community_moderators_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: community_projects community_projects_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_projects
    ADD CONSTRAINT community_projects_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_projects community_projects_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_projects
    ADD CONSTRAINT community_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.open_source_projects(id) ON DELETE CASCADE;


--
-- Name: community_projects community_projects_requested_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_projects
    ADD CONSTRAINT community_projects_requested_by_user_id_fkey FOREIGN KEY (requested_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: community_projects community_projects_reviewed_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_projects
    ADD CONSTRAINT community_projects_reviewed_by_user_id_fkey FOREIGN KEY (reviewed_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: community_realtime_generations community_realtime_generations_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_realtime_generations
    ADD CONSTRAINT community_realtime_generations_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_sections community_sections_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections
    ADD CONSTRAINT community_sections_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_user_stats community_user_stats_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_user_stats community_user_stats_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: github_installations github_installations_connected_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations
    ADD CONSTRAINT github_installations_connected_by_user_id_fkey FOREIGN KEY (connected_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: github_onboarding_states github_onboarding_states_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states
    ADD CONSTRAINT github_onboarding_states_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: mod_actions mod_actions_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions
    ADD CONSTRAINT mod_actions_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: mod_actions mod_actions_moderator_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions
    ADD CONSTRAINT mod_actions_moderator_id_fkey FOREIGN KEY (moderator_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_actor_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_actor_user_id_fkey FOREIGN KEY (actor_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: notifications notifications_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_connection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_connection_id_fkey FOREIGN KEY (connection_id) REFERENCES public.community_connections(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.open_source_projects(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_relation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_relation_id_fkey FOREIGN KEY (relation_id) REFERENCES public.community_projects(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_shared_thread_placement_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_shared_thread_placement_id_fkey FOREIGN KEY (shared_thread_placement_id) REFERENCES public.shared_thread_placements(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: open_source_projects open_source_projects_created_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.open_source_projects
    ADD CONSTRAINT open_source_projects_created_by_user_id_fkey FOREIGN KEY (created_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: open_source_projects open_source_projects_source_onboarding_draft_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.open_source_projects
    ADD CONSTRAINT open_source_projects_source_onboarding_draft_id_fkey FOREIGN KEY (source_onboarding_draft_id) REFERENCES public.project_onboarding_drafts(id) ON DELETE SET NULL;


--
-- Name: password_resets password_resets_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: post_votes post_votes_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.post_votes
    ADD CONSTRAINT post_votes_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- Name: post_votes post_votes_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.post_votes
    ADD CONSTRAINT post_votes_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: posts posts_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: posts posts_promoted_from_channel_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_promoted_from_channel_id_fkey FOREIGN KEY (promoted_from_channel_id) REFERENCES public.channels(id) ON DELETE SET NULL;


--
-- Name: posts posts_section_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_section_id_fkey FOREIGN KEY (section_id) REFERENCES public.community_sections(id) ON DELETE SET NULL;


--
-- Name: posts posts_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: project_home_audit_events project_home_audit_events_actor_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_home_audit_events
    ADD CONSTRAINT project_home_audit_events_actor_user_id_fkey FOREIGN KEY (actor_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: project_home_audit_events project_home_audit_events_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_home_audit_events
    ADD CONSTRAINT project_home_audit_events_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id);


--
-- Name: project_home_audit_events project_home_audit_events_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_home_audit_events
    ADD CONSTRAINT project_home_audit_events_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.open_source_projects(id);


--
-- Name: project_home_audit_events project_home_audit_events_relation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_home_audit_events
    ADD CONSTRAINT project_home_audit_events_relation_id_fkey FOREIGN KEY (relation_id) REFERENCES public.community_projects(id);


--
-- Name: project_onboarding_draft_repositories project_onboarding_draft_repositories_draft_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_draft_repositories
    ADD CONSTRAINT project_onboarding_draft_repositories_draft_id_fkey FOREIGN KEY (draft_id) REFERENCES public.project_onboarding_drafts(id) ON DELETE CASCADE;


--
-- Name: project_onboarding_drafts project_onboarding_drafts_github_installation_record_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_drafts
    ADD CONSTRAINT project_onboarding_drafts_github_installation_record_id_fkey FOREIGN KEY (github_installation_record_id) REFERENCES public.github_installations(id) ON DELETE RESTRICT;


--
-- Name: project_onboarding_drafts project_onboarding_drafts_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_onboarding_drafts
    ADD CONSTRAINT project_onboarding_drafts_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: project_repositories project_repositories_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_repositories
    ADD CONSTRAINT project_repositories_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.open_source_projects(id) ON DELETE CASCADE;


--
-- Name: project_stewards project_stewards_github_installation_record_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_stewards
    ADD CONSTRAINT project_stewards_github_installation_record_id_fkey FOREIGN KEY (github_installation_record_id) REFERENCES public.github_installations(id) ON DELETE RESTRICT;


--
-- Name: project_stewards project_stewards_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_stewards
    ADD CONSTRAINT project_stewards_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.open_source_projects(id) ON DELETE CASCADE;


--
-- Name: project_stewards project_stewards_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_stewards
    ADD CONSTRAINT project_stewards_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: reports reports_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: reports reports_reporter_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_reporter_user_id_fkey FOREIGN KEY (reporter_user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: reports reports_resolved_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_resolved_by_user_id_fkey FOREIGN KEY (resolved_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: reports reports_target_author_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_target_author_user_id_fkey FOREIGN KEY (target_author_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: shared_thread_placement_audit_events shared_thread_placement_audit_eve_destination_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placement_audit_events
    ADD CONSTRAINT shared_thread_placement_audit_eve_destination_community_id_fkey FOREIGN KEY (destination_community_id) REFERENCES public.communities(id);


--
-- Name: shared_thread_placement_audit_events shared_thread_placement_audit_events_actor_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placement_audit_events
    ADD CONSTRAINT shared_thread_placement_audit_events_actor_user_id_fkey FOREIGN KEY (actor_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: shared_thread_placement_audit_events shared_thread_placement_audit_events_origin_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placement_audit_events
    ADD CONSTRAINT shared_thread_placement_audit_events_origin_community_id_fkey FOREIGN KEY (origin_community_id) REFERENCES public.communities(id);


--
-- Name: shared_thread_placement_audit_events shared_thread_placement_audit_events_placement_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placement_audit_events
    ADD CONSTRAINT shared_thread_placement_audit_events_placement_id_fkey FOREIGN KEY (placement_id) REFERENCES public.shared_thread_placements(id);


--
-- Name: shared_thread_placement_audit_events shared_thread_placement_audit_events_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placement_audit_events
    ADD CONSTRAINT shared_thread_placement_audit_events_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id);


--
-- Name: shared_thread_placements shared_thread_placements_destination_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_destination_community_id_fkey FOREIGN KEY (destination_community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: shared_thread_placements shared_thread_placements_destination_section_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_destination_section_id_fkey FOREIGN KEY (destination_section_id) REFERENCES public.community_sections(id) ON DELETE SET NULL;


--
-- Name: shared_thread_placements shared_thread_placements_origin_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_origin_community_id_fkey FOREIGN KEY (origin_community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: shared_thread_placements shared_thread_placements_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- Name: shared_thread_placements shared_thread_placements_removed_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_removed_by_user_id_fkey FOREIGN KEY (removed_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: shared_thread_placements shared_thread_placements_requested_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_requested_by_user_id_fkey FOREIGN KEY (requested_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: shared_thread_placements shared_thread_placements_reviewed_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_reviewed_by_user_id_fkey FOREIGN KEY (reviewed_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: shared_thread_placements shared_thread_placements_withdrawn_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.shared_thread_placements
    ADD CONSTRAINT shared_thread_placements_withdrawn_by_user_id_fkey FOREIGN KEY (withdrawn_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: thread_source_messages thread_source_messages_message_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.thread_source_messages
    ADD CONSTRAINT thread_source_messages_message_id_fkey FOREIGN KEY (message_id) REFERENCES public.chat_messages(id) ON DELETE CASCADE;


--
-- Name: thread_source_messages thread_source_messages_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.thread_source_messages
    ADD CONSTRAINT thread_source_messages_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

\unrestrict dbmate


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20260317133145'),
    ('20260317142357'),
    ('20260317170540'),
    ('20260318120000'),
    ('20260323140417'),
    ('20260402000000'),
    ('20260511150728'),
    ('20260511200000'),
    ('20260512100028'),
    ('20260608160856'),
    ('20260609105640'),
    ('20260616120000'),
    ('20260619120000'),
    ('20260620140000'),
    ('20260622120000'),
    ('20260722120000'),
    ('20260722200000'),
    ('20260723120000'),
    ('20260723150000'),
    ('20260724120000'),
    ('20260724130000'),
    ('20260724140000'),
    ('20260726120000'),
    ('20260726130000'),
    ('20260727120000'),
    ('20260727130000'),
    ('20260731120000'),
    ('20260731130000'),
    ('20260803120000'),
    ('20260929120000');
