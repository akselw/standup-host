create type public.rotation_length as enum ('DAILY', 'WEEKLY');

create table team
(
    id              uuid            not null default uuid_generate_v4(),
    name            text            not null,
    slug       text            not null,
    rotation_length rotation_length not null default 'DAILY',
    proper_random   boolean         not null default TRUE,
    owner_id        uuid            not null references auth.users,
    constraint unique_slug unique (slug),
    constraint slug_format CHECK (slug ~* '^[a-z0-9]+(-[a-z0-9]+)*$'),
    constraint slug_blacklist CHECK (slug not in ('admin', 'settings', 'mine-team', 'login', 'oauth', 'team', 'legg-til')),
    primary key (id)
);

create table team_member
(
    id      uuid not null default uuid_generate_v4(),
    team_id uuid not null references team,
    name    text not null,
    primary key (id)
);

-- Security

alter table team
    enable row level security;

alter table team_member
    enable row level security;

CREATE POLICY "Allow-anon-team-read-access"
    ON public.team
    FOR SELECT USING (
    true
    );

CREATE POLICY "Allow-anon-team_member-read-access"
    ON public.team_member
    FOR SELECT USING (
    true
    );

CREATE POLICY "Allow-owner-team-full-access"
    ON public.team
    FOR ALL USING (
    team.owner_id = auth.uid()
    );

CREATE POLICY "Allow-owner-team_member-full-access"
    ON public.team_member
    FOR ALL USING (
    EXISTS (SELECT 1
            FROM team
            WHERE team.id = team_member.team_id
              AND team.owner_id = auth.uid())
    );

